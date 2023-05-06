library("tidyverse")
library("tidyquant")
library("lubridate")
library("readr")

bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

bike_orderlines_tbl


install.packages("groundhound")

library(devtools)
install_github("duckmayr/oldr")

oldr::install.compatible.packages("NLP")

oldr::