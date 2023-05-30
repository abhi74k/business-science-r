library(tidyverse)
library(tidyquant)
library(lubridate)
library(readxl)
library(broom)
library(zoo)  # rolling operations
library(ggrepel)
library(fs)

bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

# Read all the excel files in "data/bike_sales/data_raw/" ----

files_tbl <- fs::dir_info("data/bike_sales/data_raw/")

files_path <- files_tbl %>% pull(path)

files_path %>% map(read_excel) %>% set_names(files_path) %>% View()

#Read all the sheets from an excel file
sheets <- excel_sheets("data/bike_sales/data_raw/bikes.xlsx")
sheets %>%
  map(~read_excel("data/bike_sales/data_raw/bikes.xlsx", sheet=.x)) %>%
  set_names(sheets) %>%
  View()

# Mapping across columns ----

# Each tibble is stored as a list
# Each element in the list is a column
examples <- tibble(
  x1 = 1:3,
  x2 = 4:6
)

is.list(examples) # Returns TRUE

examples %>% map(sum) # Sum for each column

# class of each column 
bike_orderlines_tbl %>% map(~class(.)[1])
bike_orderlines_tbl %>% map_df(~class(.)[1]) %>% gather()
bike_orderlines_tbl %>% map_df(~sum(is.na(.)) / length(.)) %>% gather()

# Mapping across rows ----
examples %>% 
  mutate(sum = x1 + x2)

data_table <- files_tbl %>% 
  select(path) %>% 
  mutate(
    data = path %>% map(read_excel)
  )

data_table_unnested <-  data_table %>% 
  mutate(
    ID = row_number()
  ) %>% 
  unnest(data)

data_table_nested <- data_table_unnested %>%
  group_by(path, ID) %>% 
  nest() %>% 
  ungroup()

# We need to apply a function to each element of data column
# to filter out columns with just NA
filter_empty_cols <- function(data) {
  
  res <- data %>% 
    select_if(~!all(is.na(.x)))
  
  return(res)
}

data_table_nested_cleaned <- data_table_nested %>% 
  mutate(
    data = data %>% map(filter_empty_cols)
  )

data_table_nested_cleaned        

rolling_avg_3_tbl <- bike_orderlines_tbl %>% 
  select(order_date, category_1, category_2, total_price) %>% 
  
  mutate(order_date = ymd(order_date)) %>% 
  mutate(order_date = lubridate::ceiling_date(order_date, unit= "month") - duration(1, units = "day")) %>% 
  
  # When grouping by the third argument should be order date because
  # that's what changes per row
  group_by(category_1, category_2, order_date) %>% 
  summarise(sales = sum(total_price))  %>% 
  mutate(rolling_salesavg_3 = rollmean(sales, 3, na.pad = TRUE, align = "right")) %>% 
  ungroup() %>% 
  mutate(category_2 = as_factor(category_2) %>%  fct_reorder2(order_date, sales))


sales_by_cross_country <- rolling_avg_3_tbl %>% 
  filter(category_2 == "Cross Country Race") %>% 
  select(order_date, sales) %>% 
  mutate(
    order_date_numeric = as.numeric(order_date)
  )

# Fit the model
fit_loess_cross_country <-  sales_by_cross_country %>% 
  loess(sales ~ order_date_numeric, data = ., span = 0.2)

# Broom to massage the model output
fit_loess_cross_country %>% 
  broom::augment() %>%  # Get the fitted data as a tibble
  ggplot(aes(x = order_date_numeric, y = sales)) + 
  geom_point() + 
  geom_line(aes(y = .fitted))

# Fit loess for each of category 
fit_loess <- function(data) {
  
  loess_fitted <- data %>% 
    mutate(order_date_numeric = as.numeric(order_date)) %>% 
    loess(sales ~ order_date_numeric, data=., span=0.2)
  
  fitted <- broom::augment(loess_fitted) %>% 
            select(.fitted)
  
  return(fitted)  
}

loess_by_category <- rolling_avg_3_tbl %>%  
  group_by(category_1, category_2) %>%  
  nest() %>%
  mutate(fitted = data %>% map(fit_loess)) %>% 
  ungroup() %>% 
  unnest()
  
loess_by_category %>% 
  ggplot(aes(x = order_date, y = sales, color = category_2)) + 
  geom_point() + 
  geom_smooth(method = "loess", color="blue", span=0.2, se=FALSE, size=2) + 
  geom_line(aes(y = .fitted), size=1) +
  
  facet_wrap(~category_2, scales = "free_y")
