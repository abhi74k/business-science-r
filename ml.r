library(tidyverse)
library(tidyquant)
library(lubridate)
library(readxl)
library(zoo)  # rolling operations
library(ggrepel)
library(fs)
library(umap)

library(broom)    # pretty printing model output
library(parsnip)  # unified API for fitting ML models
library(yardstick)# model performance
library(rsample)  # test/train split 

library(rpart.plot)

bike_orderlines_tbl <- read_rds("data/bike_sales/data_wrangled/bike_sales_wrangled.rds")

model_sales_tbl <- bike_orderlines_tbl %>% 
  select(total_price, model, category_2, frame_material) %>% 
  
  group_by(model, category_2, frame_material) %>% 
  summarise(sales = sum(total_price)) %>% 
  ungroup() %>% 
  
  arrange(desc(sales))

model_sales_tbl %>% 
  # order my max sales in category_2 
  mutate(
    category_2 = as_factor(category_2) %>% fct_reorder(sales, .fun=max) %>% fct_rev()
  ) %>% 
  ggplot(aes(x=frame_material, y = sales)) +
  geom_violin() + 
  geom_jitter() + 
  facet_wrap(~category_2)


# 2.0 TRAINING & TEST SETS ----

separate_bike_model <- function(data, keep_model_column = TRUE, append = TRUE) {
  
  # Append
  if (!append) {
    data <- data %>% select(model)
  }
  
  # Pipeline
  output_tbl <- data %>%
    
    # select(model) %>%
    
    # Fix typo
    mutate(model = case_when(
      model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
      model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
      model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
      TRUE ~ model
    )) %>%
    
    # separate using spaces
    separate(col     = model, 
             into    = str_c("model_", 1:7), 
             sep     = " ", 
             remove  = FALSE, 
             fill    = "right") %>%
    
    # creating a "base" feature
    mutate(model_base = case_when(
      
      # Fix Supersix Evo
      str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Fat CAAD bikes
      str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Beast of the East
      str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
      
      # Fix Bad Habit
      str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Scalpel 29
      str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
      
      # catch all
      TRUE ~ model_1)
    ) %>%
    
    # Get "tier" feature
    mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
    
    # Remove unnecessary columns
    select(-matches("model_[0-9]")) %>%
    
    # Create Flags
    mutate(
      black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
      hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
      team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
      red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
      ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
      dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
      disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
    )
  
  if (!keep_model_column) output_tbl <- output_tbl %>% select(-model)
  
  return(output_tbl)
  
}

# Creates engineering features
bike_features_tbl <- bike_orderlines_tbl %>%
  
  select(price, model, category_2, frame_material) %>% 
  
  distinct() %>% 
  
  mutate(id = row_number()) %>% 
  
  select(id, everything()) %>% 
  
  separate_bike_model()

set.seed(12345)

split_obj <- bike_features_tbl %>% 
  rsample::initial_split(prop = 0.75, strata = "model_base")

training_set <- training(split_obj)
testing_set <- testing(split_obj)  

simple_lm <- parsnip::linear_reg(mode = "regression") %>% 
  set_engine("lm") %>% 
  fit(price ~ category_2 + frame_material, data = training_set)

# Training stats
simple_lm %>% 
  broom::tidy() %>% 
  arrange(p.value) %>% 
  mutate(isSignificant = case_when(
    p.value < 0.05 ~ "Significant", 
    TRUE ~ "Insignificant"
  ))

# Predict
simple_lm %>% 
  predict(testing_set) %>% 
  bind_cols(testing_set %>% select(price)) %>% 
  select(!contains('pred'), .pred) %>% 
  yardstick::metrics(truth=price, estimate=.pred)
  
# Feature importance plots
simple_lm %>% 
  broom::tidy() %>% 
  arrange(p.value) %>% 
  mutate(isSignificant = case_when(
    p.value < 0.05 ~ "Significant", 
    TRUE ~ "Insignificant"
  )) %>% 
  mutate(
    term = as_factor(term) %>% fct_reorder(p.value) %>% fct_rev()
  ) %>% 
  ggplot(aes(x = estimate, y = term)) + 
  geom_point() + 
  ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, suffix = 'M', accuracy =  1), size=2))

# Complex model 
complex_lm <- linear_reg(mode = "regression") %>% 
  set_engine("lm") %>% 
  fit(price ~ ., data = training_set %>% select(-id, -model, -model_tier, -model_base))

compute_metrics <- function (model, dataset) {
  model %>% 
    predict(dataset) %>% 
    bind_cols(dataset %>% select(price)) %>% 
    yardstick::metrics(truth=price, estimate=.pred)
}

model <- complex_lm
dataset <- testing_set
compute_metrics(complex_lm, training_set %>% select(-id, -model, -model_tier, -model_base))
compute_metrics(simple_lm, testing_set)
compute_metrics(complex_lm, testing_set %>% select(-id, -model, -model_tier, -model_base))

complex_lm %>% broom::tidy() %>% arrange(p.value)

# GLMNET (Combination of L1 & L2)

glmnet_model <-  linear_reg(mode = "regression", penalty = 5, mixture = 0) %>% 
  set_engine("glmnet") %>% 
  fit(price ~ ., data = training_set %>% select(-id, -model, -model_tier, -model_base))

compute_metrics(simple_lm, testing_set)
compute_metrics(complex_lm, testing_set %>% select(-id, -model, -model_tier, -model_base))
compute_metrics(glmnet_model, testing_set %>% select(-id, -model, -model_tier, -model_base))

glmnet_model %>% broom::tidy()

# DECISION TREES ----

decision_tree_model <- decision_tree(
  mode = "regression",
  cost_complexity = 0.1, 
  tree_depth = 5, 
  min_n = 5
  ) %>% 
  set_engine("rpart") %>% 
  fit(price ~ ., data = training_set %>% select(-id, -model, -model_tier, -model_base))

compute_metrics(decision_tree_model, testing_set)

# Visualize decision tree
rpart.plot::rpart.plot(decision_tree_model$fit)

# RANDOM FOREST ----
rand_forest_ranger_model <-  rand_forest(mode = "regression") %>% 
  set_engine("ranger") %>% 
  fit(price ~ ., data = training_set %>% select(-id, -model, -model_tier, -model_base))

rand_forest_mode %>% compute_metrics(testing_set)

rand_forest_ranger_model_tuned <-  rand_forest(mode = "regression", mtry = 8, trees = 1000, min_n = 10) %>% 
  set_engine("ranger", splitrule = "extratrees", importance = "impurity") %>% 
  fit(price ~ ., data = training_set %>% select(-id, -model, -model_tier, -model_base))

rand_forest_ranger_model_tuned %>% compute_metrics(testing_set)

# enframe is used to convert a named vector to tibble
rand_forest_ranger_model_tuned$fit %>% ranger::importance() %>% enframe()
rand_forest_ranger_model_tuned$fit %>% ranger::importance() %>% as_tibble(rownames = "feature")

# Different implementation of random forest 

rand_forest_randomForest_model <-  rand_forest(mode = "regression") %>% 
  set_engine("randomForest") %>% 
  fit(price ~ ., data = training_set %>% select(-id, -model, -model_tier, -model_base))

rand_forest_randomForest_model %>% compute_metrics(testing_set)

# XG Boost 
xgboost_model <- boost_tree(mode = "regression", learn_rate = 0.20) %>% 
  set_engine("xgboost") %>% 
  fit(price ~ ., data = training_set %>% select(-id, -model, -model_tier, -model_base))

xgboost_model %>% compute_metrics(testing_set)

xgboost_importance <- xgboost::xgb.importance(model = xgboost_model$fit) %>% as_tibble()

xgboost::xgb.ggplot.importance(importance_matrix = xgboost::xgb.importance(model = xgboost_model$fit))
