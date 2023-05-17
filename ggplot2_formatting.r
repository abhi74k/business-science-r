

# List of supported named colors
colors()

col2rgb("slateblue")
colors()[10] %>% col2rgb()
col2rgb("#2c3e50")

# To HEX
rgb(118, 238, 198, maxColorValue = 255)

tidyquant::palette_light()

RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(8, name = "Set3")
RColorBrewer::brewer.pal(8, name = "Set3")[10] %>% col2rgb()


viridisLite::plasma(10)


bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

revenue_by_year_category_2_tbl <- bike_orderlines_tbl %>% 
  
  select(order_date, category_2, total_price) %>% 
  
  mutate(year = year(order_date)) %>% 
  
  group_by(year, category_2) %>% 
  
  summarise(revenue = sum(total_price)) %>% 
  
  ungroup()  %>% 
  
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(revenue) %>% fct_rev())

# Line plot with 1 aesthetic 
revenue_by_year_category_2_tbl %>% 
  ggplot(aes(x=year, y = revenue, color=category_2)) + 
  
  geom_line() + 
  
  geom_point()


revenue_by_year_category_2_tbl %>% 
  ggplot(aes(x=year, y = revenue, color=category_2)) + 
  
  geom_line() + 
  
  geom_point() + 
  
  facet_wrap(~category_2, scales = "free_y", ncol = 2) + 
  
  geom_smooth(method = "lm", se = FALSE, linetype = 5)


revenue_by_year_category_2_tbl %>% 
  ggplot(aes(x=year, y = revenue, fill=category_2)) + 
  
  geom_col()

revenue_by_year_category_2_tbl %>% 
  ggplot(aes(x=year, y = revenue, fill=category_2)) + 
  
  geom_col(position = "dodge", width=0.9, color="white")

revenue_by_year_category_2_tbl %>% 
  ggplot(aes(x=year, y = revenue, fill=category_2)) + 
  
  geom_area(color="black")

revenue_by_year_category_2_tbl %>% 
  ggplot(aes(x=year, y = revenue, fill=category_2)) + 
  
  geom_area(color="black") + 
  
  scale_fill_brewer(palette = "Blues", direction=-1)


# Playing with scales ----

g_facet_continuous <-  revenue_by_year_category_2_tbl %>% 
  
  ggplot(aes(year, revenue, color = revenue)) + 
  
  geom_line(size=1) + 
  geom_point(size=3) + 
  facet_wrap(~category_2, scales = "free_y") + 
  expand_limits(y = 0) + 
  theme_minimal()

g_facet_discrete <-  revenue_by_year_category_2_tbl %>% 
  
  ggplot(aes(year, revenue, color = category_2)) + 
  
  geom_line(size=1) + 
  geom_point(size=3) + 
  facet_wrap(~category_2, scales = "free_y") + 
  expand_limits(y = 0) + 
  theme_minimal()

g_area_discrete <- revenue_by_year_category_2_tbl %>% 
  ggplot(aes(x=year, y = revenue, fill=category_2)) + 
  geom_area(color="black") + 
  theme_minimal()


g_facet_continuous + 
  
  #scale_color_continuous(low="green", high="red")
  
  scale_color_viridis_c(option = "C")


g_facet_discrete + 
  
  scale_color_viridis_d()

g_facet_discrete + 
  
  scale_color_tq()

g_facet_discrete + 
  
  scale_color_tq(theme = "da rk")

g_facet_discrete + 
  scale_color_brewer(palette = "Set3")

g_area_discrete + 
  scale_fill_brewer(palette = "Set3")

g_facet_continuous + 
  scale_y_continuous(label = scales::dollar_format()) + 
  
  scale_x_continuous(breaks = seq(2011, 2015, 2))

g_facet_continuous + 
  scale_y_continuous(label = scales::dollar_format()) + 
  
  scale_x_continuous(breaks = seq(2011, 2015, 2)) + 
  
  labs(
    x = "", ,
    y = "Millions", 
    title = "Revenue per year for category 2", 
    subtitle = "Increasing trend across products", 
    caption = "*Open source data from website", 
    color = "Revenue"
  ) + 
  
  theme(
    axis.text.x = element_text(
      angle = 45, 
      vjust=0.5
    ), 
    
    strip.background = element_rect(
      fill = "cornflowerblue"
    ), 
    
    strip.text = element_text(
      face = "bold", 
      color = "white"
    ), 
    
    # title refers to all the text on the plot
    title = element_text(
      face = "bold", 
      color = tail(RColorBrewer::brewer.pal(10, "Blues"), 1)
    )
  )

