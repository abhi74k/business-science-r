library(tidyverse)


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