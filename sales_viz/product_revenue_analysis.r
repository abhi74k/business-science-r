library("readxl")
library("tidyverse")
library("tidyquant")

# Read excel ---
bikes_tbl <- read_excel("data/bike_sales/data_raw/bikes.xlsx")
bikeshops_tbl <- read_excel("data/bike_sales/data_raw/bikeshops.xlsx")
orderlines_tbl <- read_excel("data/bike_sales/data_raw/orderlines.xlsx")

# glimpse ---
glimpse(bikes_tbl)

# left join ---
glimpse(left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id")))

# left join using glimpse ---
ordersLines_joined <- orderlines_tbl %>% 
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# separate --- 
books_ordersline_wrangled <- ordersLines_joined %>%
  
  # Split description column into 3 different columns
  # Mountain - Over Mountain - Carbon is an element from the column
  # Separater is " - "
  separate("description", into = c("category.1", "category.2", "frame.material"), sep = " - ") %>%
  
  # Split location into City, State
  # Ithaca, NY
  separate("location", into = c("city", "state"), sep=", ", remove=FALSE) %>%
  
  # Total price = quantity * price
  mutate("total.price" = price * quantity) %>%
  
  # Reorganization with select and select helpers
  select(-"...1", -"location") %>%
  
  # Remove everything which ends with id
  select(-ends_with(".id")) %>%
  
  # Add back order.id column. We use bind_cols() which is similar to 
  # hstack in numpy 
  bind_cols(orderlines_tbl %>% select("order.id")) %>%
  
  #Reorder columns
  select(contains("date"), contains("id"), contains("order"), 
         "quantity", "price", "total.price", everything()) %>%
  
  # Rename multiple columns. new_value = old_value
  rename("order_date" = "order.date", "order_id" = "order.id") %>%
  
  # Rename all columns
  set_names(names(.) %>% str_replace("\\.", "_")) 

books_ordersline_wrangled %>% View()

# Prepare data for visualization ---
# Prepare the data set to plot sales by year
# Year, Sales 

sales_by_year <- books_ordersline_wrangled %>%
  
  # Select order_date and total_price
  select(order_date, total_price) %>%
  
  # Create a near column with only year
  mutate(year = year(order_date)) %>%
  
  # Group by year and summarize sales
  group_by(year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Add a new column sales_text which is dollar formatted
  mutate(sales_txt = scales::dollar(sales))

sales_by_year %>% View()

# Visualization -- 

sales_by_year %>%
  
  # Create a canvas. aes(Aesthetic) aes maps data to visual properties
  ggplot(aes(x = year, y = sales)) + 
  
  # Bar chart. fill is not in aes because color is not inferred from data
  geom_col(fill="#A6CEE3") + 
  
  # Label points in scatter plot or height of bar
  geom_label(aes(label=sales_txt)) + 
  
  # Draw a trend line
  geom_smooth(method = 'lm', se = FALSE) + 
  
  # Tidyquant theme for a professional look
  theme_tq() + 
  
  # Each y label is transformed
  scale_y_continuous(labels = scales::dollar) + 
  
  # Labels for the plot
  labs(
    x = "", y = "Revenue",
    title = "Revenue by year", 
    subtitle = "Increasing trend", 
  )

# Visualize revenue by year for category 2 -- 

# Prepare the data
# Group by year, category 2 and compute sum of sales
# year, category 2, sales
books_year_category2_grouped_tbl <- books_ordersline_wrangled %>% 
  
  select(order_date, category_2, total_price) %>%
  
  mutate(year = year(order_date)) %>%
  
  group_by(year, category_2) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_text = scales::dollar(sales)) 

books_year_category2_grouped_tbl %>%
  ggplot(aes(x = year, y = sales, fill = category_2)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::dollar) + 
  theme_tq() + 
  scale_fill_tq() + 
  labs(x = "", fill = "Product category") 
  
books_year_category2_grouped_tbl %>%
  ggplot(aes(x = year, y = sales, fill = category_2)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::dollar) + 
  theme_tq() + 
  scale_fill_tq() + 
  facet_wrap("category_2", scales="free") +
  geom_smooth(method = "lm", se=FALSE) + 
  labs(x = "", fill = "Product category") 

