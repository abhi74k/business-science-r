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
 
# The goal is to create customer-product matrix 
customer_trends_tbl <-  bike_orderlines_tbl %>% 
  
  # bikeshop_name is the customer
  # price, model, category_1, category_2,  frame_material are product attributes
  select(bikeshop_name, price, model, category_1, category_2,  frame_material, quantity) %>% 
  group_by(bikeshop_name, price, model, category_1, category_2,  frame_material) %>% 
  summarise(total_purchased = sum(quantity)) %>% 
  ungroup() %>% 
  
  group_by(bikeshop_name) %>% 
  mutate(prop_of_total = total_purchased / sum(total_purchased)) %>% 
  ungroup()
  
  
customer_product_tbl <-  customer_trends_tbl %>% 
  select(bikeshop_name, model, prop_of_total) %>% 
  spread(key = model, value = prop_of_total, fill = 0)

kmeans_fit <- customer_product_tbl %>% 
  select(-bikeshop_name) %>% 
  kmeans(centers = 4, nstart = 100)

broom::glance(kmeans_fit)

broom::tidy(kmeans_fit)

# Attaches .cluster column
clustered_res <- broom::augment(kmeans_fit, customer_product_tbl) %>% 
  select(bikeshop_name, .cluster)

# Skree plot (Attempt #2)
kmeans_model_fitter <- function(nclusters) {
  
  tot_withinss <- customer_product_tbl %>% 
    select(-bikeshop_name) %>% 
    kmeans(centers = nclusters, nstart = 100) %>% 
    broom::glance()
  
  return(tot_withinss$tot.withinss)
}

skree_plot_data <-  tibble(nclusters = 1:10) %>% 
  mutate(tot_ss = nclusters %>% map_dbl(kmeans_model_fitter))

skree_plot_data %>% 
  ggplot(aes(x = nclusters, y = tot_ss)) +
  geom_point(size=2) + 
  geom_line()
  
#Attempt #2 to build the dataset
kmeans_model_fitter <- function(nclusters) {
  
  customer_product_tbl %>% 
    select(-bikeshop_name) %>% 
    kmeans(centers = nclusters, nstart = 100)
}

tibble(nclusters = 1:10) %>% 
  mutate(k_means = nclusters %>% map(kmeans_model_fitter)) %>% 
  mutate(glance = k_means %>% map(broom::glance)) %>% 
  unnest(glance)

umap_model <- customer_product_tbl %>% 
  select(-bikeshop_name) %>% 
  umap()
 
manifold_2d <- umap_model$layout %>% 
  as_tibble() %>% 
  set_names(c("x", "y"))  %>% 
  bind_cols(customer_product_tbl %>% select(bikeshop_name))


final_result <- clustered_res %>% 
  left_join(manifold_2d)

final_result %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  ggrepel::geom_label_repel(aes(label = bikeshop_name, color=.cluster), size=2)

customer_trends_with_cluster <- customer_trends_tbl %>% 
  left_join(final_result)

# Create price bin

customer_trends_with_cluster %>% pull(price) %>% quantile(c(.33, 0.66))

cluster_trends_tbl <-  customer_trends_with_cluster %>% 
  
  # Create price bin
  mutate(price_bin = case_when(
    price <= 2240 ~ "low",
    price <= 4260 ~ "med",
    TRUE ~ "high"
  )) %>% 
  
  # Re-arrange and drop columns which are not required
  # The columns contain information about each cluster
  select(.cluster, model, contains("price"), category_1:total_purchased) %>% 
  
  # Aggregate data based on cluster and product attributes
  group_by_at(.vars = vars(.cluster:frame_material)) %>% 
  summarise(total_quantity = sum(total_purchased)) %>% 
  ungroup() %>% 
  
  group_by(.cluster) %>% 
  mutate(prop_of_total = total_quantity / sum(total_quantity)) %>% 
  ungroup()

get_cluster_trends <- function(cluster_id) {
  cluster_trends_tbl %>% 
    filter(.cluster == cluster_id) %>% 
    arrange(desc(prop_of_total)) %>% 
    mutate(cumsum = cumsum(prop_of_total))
}

# Low/Med Mountain bikes, Aluminium frame
get_cluster_trends(1)

# Med/High Mountain bikes, Carbon frame
get_cluster_trends(2)

# Med/High Road bikes, Carbon frame
get_cluster_trends(3)

# Low/Med Road
get_cluster_trends(4)



