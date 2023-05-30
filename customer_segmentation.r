library(tidyverse)
library(tidyquant)
library(lubridate)
library(readxl)
library(broom)
library(zoo)  # rolling operations
library(ggrepel)
library(fs)
library(umap)

bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")
