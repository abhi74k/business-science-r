library("tidyverse")
library("tidyquant")
library("lubridate")

bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

# Anatomy of ggplot ----

# Revenue by year
revenue_by_year <- bike_orderlines_tbl %>%
  
  # Selecting the year and total_price column
  select(order_date, total_price) %>% 
  
  # Extracting year from order_date
  mutate(year = year(order_date)) %>% 
  
  # Group_by and aggregate 
  group_by(year) %>% 
  summarize(revenue = sum(total_price)) %>% 
  ungroup()


# Plot 

revenue_by_year %>% 
  
  # Canvas
  ggplot(aes(x = year, y = revenue, color = revenue)) + 
  
  # Geometries
  geom_line(size= 1) + 
  #geom_point(aes(size = revenue)) + #Affects size of only points
  geom_point(size = 5) + #Affects size of only points
  geom_smooth(method = "lm", se = FALSE) + 

  # Formatting 
  expand_limits(y = 0) + 
  
  scale_color_continuous(low = "red", high="green", label=scales::dollar_format()) + 
  
  scale_y_continuous(label=scales::dollar_format()) + 
  
  theme_bw() + 
  
  labs(
    title = "Revenue by year", 
    subtitle = "A increasing trend", 
    x = "", 
    y = "Revenue(Millions)"
  )

# Scatter plots ----
# Good for continuous vs continuous feature
# Total quantity vs Order value

total_qty_and_revenue <- bike_orderlines_tbl %>% 
  select(order_id, quantity, total_price) %>% 
  group_by(order_id) %>% 
  summarise(
    total_quantity = sum(quantity), 
    order_value = sum(total_price)
  ) %>% 
  ungroup() %>% 
  select(-order_id)

total_qty_and_revenue %>% 
  ggplot(aes(x = total_quantity, y = order_value)) + 
  
  geom_point(alpha = 0.5) + 
  geom_smooth(method= "lm", se = FALSE)


# Line plot ----

revenue_by_year_tbl <- bike_orderlines_tbl %>% 
  select(order_date, total_price) %>% 
  
  mutate(year_month = floor_date(order_date, "month") %>% ymd()) %>% 
  
  group_by(year_month) %>% 
  
  summarise(revenue = sum(total_price)) %>% 
  
  ungroup()
         
revenue_by_year_tbl %>% 
  
  ggplot(aes(x = year_month, y = revenue)) + 
  
  geom_line(linetype = 1) + 
  
  geom_smooth(span=0.2) #loess by default

# Column plot (Categorical vs Continuous) ----
# Bar plot is for 1D

revenue_by_category_2_tbl <- bike_orderlines_tbl %>% 
  
  select(category_2, total_price) %>% 
  
  group_by(category_2) %>% 
  
  summarise(revenue = sum(total_price)) %>% 
  
  ungroup()

revenue_by_category_2_tbl %>% 
  
  ggplot(aes(y = as_factor(category_2) %>% fct_reorder(revenue), x = revenue)) + 
  
  geom_col(fill = "blue") + 
  
  scale_x_continuous(label = scales::dollar_format()) + 
  
  labs(
    y = "category_2"
  )

# Histogram ----
# 1D distribution
# Plot unit price of models

bike_orderlines_tbl %>% 
  
  distinct(model, price) %>% 
  
  ggplot(aes(price)) +
  
  geom_histogram(bins=20, fill = "blue", color="white")
  

# Faceted Histogram ----

bike_orderlines_tbl %>% 
  
  distinct(model, frame_material, price) %>% 
  
  ggplot(aes(price, fill = frame_material)) + 
  
  geom_histogram() + 
  
  facet_wrap(~ frame_material, ncol = 1) + 
  
  scale_fill_tq() + 
  
  theme_tq()

# Density plot ----

bike_orderlines_tbl %>% 
  
  distinct(model, frame_material, price) %>% 
  
  ggplot(aes(price, fill = frame_material)) + 
  
  geom_density(alpha = 0.5)

# Box plot ----
# Useful for comparing dist for different categorical variables

# Unit price of model segmenting by category 2 

bike_orderlines_tbl %>% 
  
  select(model, category_2, price) %>% 
  
  distinct() %>% 
  
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price)) %>% 
  
  ggplot(aes(category_2, price)) + 
  
  geom_boxplot() + 
  
  coord_flip()

# Violin plots ----

bike_orderlines_tbl %>% 
  
  select(model, category_2, price) %>% 
  
  distinct() %>% 
  
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price)) %>% 
  
  ggplot(aes(category_2, price)) + 
  
  geom_jitter(width = 0.15, color="blue") + 
  
  geom_violin() + 
  
  coord_flip()


# Text & Labels ----

revenue_by_year %>% 
  
  ggplot(aes(x = year, y = revenue)) + 
  
  geom_col(fill = "blue") + 
  
  geom_text(aes(label = scales::dollar(revenue, scale = 1e-6, suffix="M")), 
            color = "white", 
            vjust=2) +
  
  geom_label(label = "Biggest sales", 
             data = revenue_by_year %>% filter(year == 2015), 
             vjust=-0.1, 
             fill = "#6A3D9A", 
             color = "white", 
             fontface = "italic")

  theme_tq() + 
  
  scale_y_continuous(labels = scales::dollar_format()) + 
  
  expand_limits(y = 2e7)
