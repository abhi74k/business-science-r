library(tidyverse)
library(tidyquant)
library(lubridate)

bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

# Lollipop chart ----
# Q: How much purchasing power is in the top 5 customers
# Goal: Visualize top N customers in terms of revenue and cumulative %

n <-  7

top_customers_tbl <-  bike_orderlines_tbl %>% 
  
  select(bikeshop_name, total_price) %>% 
  
  group_by(bikeshop_name) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>% 
  
  mutate(bikeshop_name = as_factor(bikeshop_name) %>% fct_reorder(revenue) %>% fct_lump_n(n = n, w = revenue)) %>% 
  
  group_by(bikeshop_name) %>% 
  summarise(revenue = sum(revenue)) %>% 
  ungroup() %>% 
  
  mutate(bikeshop_name = bikeshop_name %>% fct_relevel("Other", after=0)) %>% 
  
  arrange(desc(bikeshop_name)) %>% 
  
  mutate(
    rank = 1:(n+1),
    cumsum_pct = scales::percent(cumsum(revenue) / sum(revenue)),
    revenue_str = scales::dollar(revenue, scale = 1e-6, suffix = "M"),
    label_str = str_glue("Rank:{rank}\nRevenue:{revenue_str},\nCumSum:{cumsum_pct}")
  )  
  
top_customers_tbl %>% 
  ggplot(aes(y = bikeshop_name, x = revenue)) +
  
  geom_point(aes(size = revenue)) + 
  
  geom_segment(aes(xend = 0, yend=bikeshop_name), size=1)  + 
  
  geom_label(aes(label = label_str, hjust = "inward"), size = 3) + 
  
  theme_tq() + 
  
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) + 
  
  labs(
    x = "Revenue($)", 
    y = "Customer",
    caption = "Top 7 customers, generate 55% of the revenue"
  ) + 
  
  theme(
    legend.position = "none",
    plot.caption = element_text(face = "bold")
  )  


# Heat map ----
# This is useful if you want to visualize 2D data or 3D using facet wraps. 
# The third dimension is categorical. To do this we need to summarize by 
# 3 dimensions. 

pct_sales_by_customer <-  bike_orderlines_tbl %>% 
  select(bikeshop_name, category_1, category_2, quantity) %>% 
  
  group_by(bikeshop_name, category_1, category_2) %>% 
  summarise(total_quantity = sum(quantity)) %>% 
  ungroup() %>% 
  
  group_by(bikeshop_name) %>% 
  mutate(pct = total_quantity / sum(total_quantity)) %>% 
  ungroup() %>% 
  
  mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev())


pct_sales_by_customer %>% 
  
  ggplot(aes(category_2, bikeshop_name)) + 
  
  geom_tile(aes(fill = pct)) + 
  
  scale_fill_gradient(low = "white", high = palette_light()[1]) + 
  
  facet_wrap(~category_1, scales = "free_x") + 
  
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), size = 3) + 
  
  theme_tq() + 
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none"
  )
  




  



  
 
  
 
