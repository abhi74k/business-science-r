library(tidyverse)
library(lubridate)


bikes_tbl <- read_excel("data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl <- read_excel("data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

# Selecting columns with select() ---

# Select using column names
bike_orderlines_tbl %>%
  select(order_date, order_id, order_line)

# Select the first 3 cols using positions
bike_orderlines_tbl %>%
  select(1:3)

# Select using select helper
bike_orderlines_tbl %>%
  select(starts_with("order"))

# Rearrange columns
bike_orderlines_tbl %>%
  select(bikeshop_name:state, everything()) %>%
  glimpse()

# Pull the contents of column to convert to a vector
bike_orderlines_tbl %>%
  pull(price) %>%
  mean()

# select_if/where
# Used to select columns based on a function. Use where() selection helper
bike_orderlines_tbl %>%
  select(where(is.numeric)) %>%
  glimpse()
          
# Syntax using formula
bike_orderlines_tbl %>%
  select(where(~ !is.numeric(.))) %>%
  glimpse()

# Syntax using anonymous function
bike_orderlines_tbl %>%
  select(where(function(x) is.numeric(x))) %>%
  glimpse()

# Arranging data with arrange() and desc() ---

# Ascending order
bikes_tbl %>%
  select(model, price) %>%
  arrange(price)

# Descending order
bikes_tbl %>%
  select(model, price) %>%
  arrange(desc(price))


### Working with rows --

bikes_tbl %>%
  slice(1:10)

bikes_tbl %>%
  filter(price > 5000)

bikes_tbl %>%
  filter(price > mean(price))

bikes_tbl %>%
  filter(price > 5000 | price < 1000)

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)

bikes_tbl %>%
  slice((nrow(.)-5):nrow(.))

bikes_tbl %>%
  filter(price > 5000, model %>% str_detect("Super"))


### Working in categories

bike_orderlines_tbl %>%
  filter(category_1 == "Mountain")

bike_orderlines_tbl %>%
  filter(category_1 != "Mountain")

bike_orderlines_tbl %>%
  filter(category_1 %in% c("Road", "Mountain"))

bike_orderlines_tbl %>%
  filter(!(category_1 %in% c("Road")))


bike_orderlines_tbl %>%
  distinct(category_1)


bike_orderlines_tbl %>%
  distinct(model, city, state)


# Adding boolean column
bike_orderlines_prices_tbl <- bike_orderlines_tbl %>%
  select(model, quantity, price, total_price) %>%
  mutate(is_supersix = model %>% str_to_lower() %>% str_detect("supersix")) %>%
  filter(is_supersix) %>%
  glimpse()

# ntile function: To convert continuous value to bins
bike_orderlines_prices_tbl %>%
  mutate(price_bin = price %>% ntile(2))

# Numeric to categorical 
bike_orderlines_tbl %>%
  select(model, quantity, price, total_price) %>%
  mutate(total_price_binned = case_when(
    total_price < total_price %>% quantile(0.25) ~ 'Low', 
    total_price > total_price %>% quantile(0.75) ~ 'High',
    TRUE ~ 'Med'
  )) %>%
  glimpse()

#Text to categorical
bike_orderlines_tbl %>%
select(model, quantity, price, total_price) %>%
mutate(model_category = case_when(
  model %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix", 
  model %>% str_to_lower() %>% str_detect("jekyll") ~ "Jekyll", 
))

# Summary functions
bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    count = n(), ## Gives the current group size
    avg = mean(total_price),
    med = median(total_price),
    sd = sd(total_price), 
    min = min(total_price),
    max = max(total_price)
  ) %>%
  arrange(desc(count))

# summarize all - detect missing values
bike_orderlines_missing_tbl <- bike_orderlines_tbl %>%
  mutate(
   total_price = c(rep(NA, 4), total_price[5:nrow(.)])
  )

# Detect number/percentage of missing values
bike_orderlines_missing_tbl %>%
  summarise_all(function(x) sum(is.na(x)))

bike_orderlines_missing_tbl %>%
  summarise_all(~ sum(is.na(.)))

bike_orderlines_missing_tbl %>%
  summarise_all(~ sum(is.na(.)) / length(.))

# Handling missing values

# Remove NA rows

bike_orderlines_missing_tbl %>%
  filter(!is.na(total_price))

bike_orderlines_missing_tbl %>%
  filter(!is.na(total_price))

# Replace NA 

bike_orderlines_missing_tbl %>%
  replace_na(list(total_price = 0, quantity = 0))

bike_orderlines_missing_tbl %>%
  mutate(total_price = total_price %>% replace_na(0))

bike_orderlines_missing_tbl %>%
  select(total_price) %>% replace_na(list(total_price = 0))
  
# Rename column 
bike_orderlines_missing_tbl %>%
  rename(
    sales = total_price,
    bike_category = category_1
  )

# Rename all columns
bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    sales = sum(total_price)
  ) %>%
  ungroup() %>%
  arrange(desc(sales)) %>%
  set_names(c("Bike Category", "Sub Bike Category", "Revenue"))

# Remove underscore from column names
bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    sales = sum(total_price)
  ) %>%
  ungroup() %>%
  arrange(desc(sales)) %>%
  set_names(names(.) %>% str_replace('_', ' ') %>% str_to_title())

# Pivoting 
# Spread : Long to wide (Reader friendly)
# Gather : Wide to long (For fitting models i.e one column contains all categories)

bike_shop_revenue <- bike_orderlines_tbl %>%
  select(category_1, category_2, total_price) %>%
  group_by(category_1, category_2) %>%
  summarise(total_price = sum(total_price)) %>%
  set_names(c("Bikeshop name", "Product", "Revenue"))

bike_shop_revenue %>%
  spread(key = "Bikeshop name", value = Revenue)


# Joining data

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))


# bind_columns
orderlines_tbl %>%
  select(-order.id) %>%
  bind_cols(orderlines_tbl %>% select(order.id))


#bind rows
train_tbl <- orderlines_tbl %>%
  slice(1:(nrow(.)/2))

test_tbl <- orderlines_tbl %>%
  slice((nrow(.)/2):nrow(.))

train_tbl %>% 
  bind_rows(test_tbl)

# separate & unite

bike_orderlines_tbl %>%
  select(order_date) %>%
  mutate(order_date = as.character(order_date)) %>%
  separate(order_date, into=c("year", "month", "day"), sep='-', remove=FALSE) %>%
  mutate(
    year  = as.double(year), 
    month = as.double(month), 
    day   = as.double(day)
  ) %>%
  unite(order_date_unified, year, month, day, remove=FALSE, sep='-') %>%
  mutate(
    order_date_unified = as.Date(order_date_unified)
  )

# lubridate ----

"2023-05-05" %>% ymd()
"2023-05-05 16:00:00" %>% ymd_hms(tz = 'America/New_York')
"1 Feb, 2023" %>% dmy()

orderlines_tbl %>%
  select(order.date) %>%
  mutate(order_date_chr = as.character(order.date)) %>%
  mutate(order_date_chr = order_date_chr %>% str_c(" 13:05:30")) %>%
  mutate(order_date_chr = order_date_chr %>% ymd_hms()) %>%
  mutate(year = order_date_chr %>% year()) %>%
  mutate(month = order_date_chr %>% month(label = TRUE)) %>%
  mutate(day = order_date_chr %>% day()) %>%
  mutate(hour = order_date_chr %>% hour()) %>%
  mutate(min = order_date_chr %>% minute()) %>%
  mutate(secs = order_date_chr %>% second()) %>%
  mutate(order_date_chr_new = str_c(as.character(year), "-", as.character(as.integer(month)), "-", as.character(day))) 

# Current time
now()

# Today's date
today()

# period(accounts for leap year etc)
"20 Feb 2024" %>% dmy() + days(10)
"20 Feb 2024" %>% dmy() + ddays(10)

orderlines_tbl %>%
  mutate(delivery_date = now()) %>%
  mutate(elapsed_days = interval(order.date, delivery_date) / ddays(1)) %>%
  mutate(elapsed_months = interval(order.date, delivery_date) / dmonths(1)) %>%
  glimpse()

# Time series aggregation ----

# Aggregated total price by year

bike_orderlines_tbl %>%
  select(order_date, total_price) %>%
  mutate(
    order_date = as.Date(order_date), 
    year = year(order_date), 
    month = month(order_date)
  ) %>%
  group_by(year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales =  scales::dollar(sales))

# Aggregated total price by month

bike_orderlines_tbl %>%
  select(order_date, total_price) %>%
  mutate(
    order_date = as.Date(order_date), 
    year = year(order_date), 
    month = month(order_date, label = TRUE)
  ) %>%
  group_by(month) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales =  scales::dollar(sales))

# Aggregate by year month using floor_date()

bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(ym = floor_date(order_date,  unit = "month")) %>%
  
  group_by(ym) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales = scales::dollar(sales)) %>%
  View()

# Lead(Push up) & Lag(Push down) operators

# day over day Pct change 
bike_orderlines_tbl %>%
  select(order_date, total_price) %>%
  mutate(total_price_lag_1 = lag(total_price, 1)) %>%
  mutate(total_price_lag_1 = case_when(
    is.na(total_price_lag_1) ~ total_price,
    TRUE ~ total_price_lag_1
  )) %>%
  mutate(
    pct_diff = (total_price - total_price_lag_1) / (total_price_lag_1)
  ) %>%
  mutate(
    pct_diff = scales::percent(pct_diff, 0.01)
  )
  
# month over month Pct change 
bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(ym = floor_date(order_date,  unit = "month")) %>%
  
  group_by(ym) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_lag_1 = lag(sales, 1)) %>%
  mutate(sales_lag_1 = case_when(
    is.na(sales_lag_1) ~ sales,
    TRUE ~ sales_lag_1
  )) %>%
  
  mutate(
    pct_diff = (sales - sales_lag_1) / (sales_lag_1)
  ) %>%
  mutate(
    pct_diff = scales::percent(pct_diff, 0.01)
  )


# Pct change year or year since inception
bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year = floor_date(order_date,  unit = "year")) %>%
  
  group_by(year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(
    pct_diff = (sales - first(sales)) / (first(sales))
  ) %>%
  mutate(
    pct_diff = scales::percent(pct_diff, 0.1)
  )

# Pct change month over month measured from start of the year. 
# IMPORTANT use of group by function 

bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(ym = floor_date(order_date,  unit = "month")) %>%
  
  group_by(ym) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(
    year  = year(ym)
  ) %>%
  
  group_by(year) %>%
  mutate(sales_ref = first(sales)) %>%
  mutate(diff = sales - sales_ref) %>%
  mutate(ratio_diff = diff / sales_ref) %>%
  mutate(pct_diff = scales::percent(ratio_diff, 0.1)) %>%
  ungroup() %>%
  
  View()

# Rolling window calculations ----
# Zoo package
library(tidyquant)

bike_orderlines_tbl %>%
  select(order_date, total_price) %>%
  mutate(
    order_date = as.Date(order_date), 
    year = year(order_date), 
    month = month(order_date, label = TRUE)
  ) %>%
  group_by(month) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_mean_3 =  zoo::rollmean(sales, 3, na.pad = TRUE, align = "right", fill = 0))
  
# Filter time series

bike_orderlines_tbl %>%
  
  mutate(order_date = as.Date(order_date)) %>%
  
  filter(order_date %>% between(ymd("2011-01-01"), ymd("2011-01-31"))) %>%
  
  View()

bike_orderlines_tbl %>%
  
  mutate(order_date = as.Date(order_date)) %>%
  
  filter(year(order_date) %in% c(2011, 2012)) %>%
  
  View()