library(tidyverse)
library(readxl)
library(writexl)

# Bike shop name, Mountain, Road, Total
bikeshop_revenue_wide_tbl <- read_excel("data/bike_sales/data_raw/bikeshop_revenue_formatted_wide.xlsx")

# Convert to long format
# Mountain and Road are type of bikes. To create a single category column
# which can be used in regression, we use the gather method

bikeshop_revenue_long_tbl <- bikeshop_revenue_wide_tbl %>% 
  select(-Total) %>%
  gather(key = "category_1", value="sales", Mountain, Road)


write_xlsx(bikeshop_revenue_long_tbl, "data/bike_sales/data_wrangled/bikeshop_revenue_formatted_long.xlsx")