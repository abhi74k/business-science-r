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

# str_glue
a = 1
b = 2
str_glue("a:{a}, b:{b}")

bike_orderlines_tbl %>%
  mutate(desc = str_glue("{quantity},{price}")) %>%
  View()

# str_split(not good). tidyr::separate is good. We already saw an example
# str_trim
bike_orderlines_tbl %>%
  select(order_id, model, category_1) %>%
  mutate(desc = str_glue("  {model},{category_1}  ")) %>%
  mutate(desc = str_trim(desc)) %>%
  separate("desc", into = c("model", "category_1"), sep = ",")

# str_replace
"fox ate a fox" %>% str_replace_all("fox", "cat")

bike_orderlines_tbl %>%
  select(order_id, model, category_1) %>%
  mutate(category_1 = str_replace_all(category_1, "Mountain", "Hill"))
  

# set_names
bike_orderlines_tbl %>%
  set_names(names(.) %>% str_to_upper())

# postfix the column names
bike_orderlines_tbl %>%
  set_names(str_glue("{names(.)}_1") )

bike_orderlines_tbl %>%
  rename(
    order_date_1 = order_date
  )

bike_orderlines_tbl %>%
  rename_at( .vars = vars(model:frame_material), 
             .funs = ~ str_glue("{.}_prod")
  ) %>%
  select(contains("_prod"))
