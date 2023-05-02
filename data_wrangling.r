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
