library(tidyverse)
library(tidyquant)
library(lubridate)
library(readxl)
library(broom)
library(zoo)  # rolling operations
library(ggrepel)

# Compute mean with trimming (10% of the smallest and largest values removed)
x <- c(1:10, 50, NA_real_)
mean(x, na.rm= TRUE, trim = 0.1)


bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

# Calculate 3 month rolling average of total_price for category_1 and category_2 
# with the date aligned at the last day of the month

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

rolling_avg_3_tbl %>% 
  ggplot(aes(x = order_date, y = sales, color = category_2)) +
  
  geom_point() + 
  geom_line(aes(y = rolling_salesavg_3)) + 
  facet_wrap(~category_2) 


x <- c(1:10, 50, NA_real_)

find_outliers <- function(x) {
  
  tbl <-  tibble(data = x)
  
  summary <- tbl %>% 
    summarise(
      q25 = quantile(data, probs = 0.25, na.rm = TRUE), 
      q75 = quantile(data, probs = 0.75, na.rm = TRUE),
      iqr = IQR(data, na.rm = TRUE), 
      lo = q25 - 1.5 * iqr, 
      hi = q75 + 1.5 * iqr
    )
  
  res <- tbl %>% 
    mutate(
      outlier = case_when(
        data < summary$lo ~ TRUE, 
        data > summary$hi ~ TRUE, 
        TRUE ~ FALSE
      )
  )
  
  return(res %>% pull(outlier))
}  

bikes_tbl <- bike_orderlines_tbl %>% 
  distinct(model, category_1, price)


bikes_tbl <- bikes_tbl %>% 
  group_by(category_1) %>% 
  mutate(
    outlier = find_outliers(price)
  ) %>% 
  ungroup

bikes_tbl %>% 
  ggplot(aes(category_1, price)) + 
  geom_boxplot() + 
  ggrepel::geom_label_repel(aes(label=model), data = . %>% filter(outlier), color="red", size=3)
