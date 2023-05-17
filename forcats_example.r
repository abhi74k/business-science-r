library("tidyverse")
library("tidyquant")

bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

sales_by_cat_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, total_price) %>%
  
  group_by(category_2) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  arrange(desc(sales)) %>%
  
  # as_factor and not as.factor
  # as,factor assigns numeric values according to lexicographic ordering
  # fct_rev takes a factor as input
  mutate(category_2 = category_2 %>% as_factor() %>% fct_rev())

plot_sales <- function(data) {
  data %>% 
    ggplot(aes(x = sales, y = category_2)) + 
    geom_point(size = 5) + 
    scale_x_continuous(labels = scales::dollar_format()) + 
    theme_tq() + 
    expand_limits(x = 0)
}


sales_by_cat_2_tbl %>% plot_sales

# Basics ----

sales_by_cat_2_tbl %>%
  mutate(
    category_2_chr = category_2 %>% as.character(),
    category_2_dbl = category_2 %>% as.numeric()
    )

sales_by_cat_2_tbl %>% pull(category_2) %>% levels()

sales_by_cat_2_tbl %>% pull(category_2) %>% levels()


sales_by_cat_2_tbl %>%
  mutate(
    category_2 = category_2 %>% fct_reorder(sales), 
    category_2_value = category_2 %>% as.numeric()
  )

### fct_reorder2 ----

sales_by_cat_2_q_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_2, total_price) %>%
  mutate(
    order_date = order_date %>% floor_date("quarter") %>% ymd()
  ) %>%
  
  group_by(category_2, order_date) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup()

sales_by_cat_2_q_tbl %>%
  ggplot(aes(x = order_date, y = sales, color=category_2)) + 
  geom_point() + 
  geom_line()

sales_by_cat_2_q_tbl %>%
  mutate(
    category_2 = category_2 %>% fct_reorder(sales),
    category_2_values = category_2 %>% as.integer()
  ) %>%
  View()
  
sales_by_cat_2_q_tbl %>%
  mutate(
    category_2 = category_2 %>% fct_reorder2(order_date, sales),
    category_2_values = category_2 %>% as.integer()
  ) %>%
  View()
  ggplot(aes(x = order_date, y = sales, color=category_2)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~category_2) + 
  
  # adjusts the color specified in aes
  scale_color_tq() + 
  theme_tq()
  
  
  # Create a sample data frame
  df <- data.frame(
    group = factor(c("B", "B", "A", "C", "C")),
    value1 = c(2, 4, 1, 5, 3),
    value2 = c(1, 3, 2, 5, 4)
  )
  
  # Reorder the levels of the 'group' variable based on 'value1' and 'value2'
  df <- df %>% mutate(
    group = fct_reorder2(group, value1, value2),
    value = group %>% as.integer()
    )
  
  # Print the reordered levels of the 'group' variable
  levels(df$group)
  