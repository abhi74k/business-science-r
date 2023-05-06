library("tidyverse")
library("lubridate")

bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

bikes_tbl <-  read_xlsx("data/bike_sales/data_raw/bikes.xlsx")

# 1.0 Basics ----

# str_detect
bike_orderlines_tbl %>%
  select(model) %>%
  mutate(
    supersix = model %>% str_detect("Supersix") %>% as.integer(),
    black = model %>% str_detect("Black") %>% as.integer()
  ) %>%
  View()

# str_c
str_c("Hi", " Abhinav")

bike_orderlines_tbl %>%
  select(model) %>%
  mutate(
    model = str_c(model, " Test")
  )
