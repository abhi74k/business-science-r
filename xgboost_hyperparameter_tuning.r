library(tidyverse)
library(tidymodels)
library(xgboost)
library(dplyr)
library(ggplot2)
library(parsnip)
library(forcats)
library(rsample)
library(doParallel)
library(dials)
library(tune)
library(workflows)
library(vip)
library(yardstick)

churn_tbl <-  read.csv("data/Churn_Modelling.csv")

# Predict churn (0:exited) i.e the customer cancelled the account
# This is a binary classification problem 

churn_features_tbl <- churn_tbl %>% 
  select(-RowNumber, -CustomerId, -Surname) %>% 
  mutate(
    Exited = Exited %>% as.factor()
  )

# Is class label balanced
churn_features_tbl %>% 
  count(Exited)

train_split_obj <- initial_split(churn_features_tbl, prop = 0.80, strata = Exited)
train_set <- training(train_split_obj)
test_set <- testing(train_split_obj)

# How the parsnip parameters are mapped to params of xgboost libaray
parsnip::get_model_env()$boost_tree_args %>% filter(engine == "xgboost") %>% select(engine, parsnip, original)

# engine  parsnip        original        
# xgboost tree_depth     max_depth       
# xgboost trees          nrounds         
# xgboost learn_rate     eta             
# xgboost mtry           colsample_bynode
# xgboost min_n          min_child_weight
# xgboost loss_reduction gamma           
# xgboost sample_size    subsample       
# xgboost stop_iter      early_stop 

# Create XGBoost Spec with tunable parameters

xgb_spec <-  boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(), loss_reduction = tune(), #model complexity
  sample_size = tune(), mtry = tune(),
  learn_rate = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

xgb_grid <-  grid_latin_hypercube(
  tree_depth(),
  learn_rate(),
  finalize(mtry(),train_set),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 30 # 30 hyperparameters
) 

xgb_wf <-  workflow() %>% 
  add_formula(Exited ~ .) %>% # This is a preprocessor. A receipe can be added
  add_model(xgb_spec)

set.seed(123)
k_folds <- vfold_cv(train_set, 5, strata = "Exited")

doParallel::registerDoParallel()

xgb_tuned_grid <-  tune_grid(
  xgb_wf, 
  resamples = k_folds, 
  grid = xgb_grid,
  control = control_grid(save_pred = T)
)

xgb_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  select(mean, mtry:sample_size) %>% 
  pivot_longer(mtry:sample_size, names_to= "parameter", values_to="value") %>% 
  ggplot(aes(x=value, y = mean, color = parameter)) +
  geom_point() + 
  facet_wrap(~parameter, scales = "free") + 
  labs(x = NULL, y = "AUC")


show_best(xgb_tuned_grid, "roc_auc")

best_hyperparams <- select_best(xgb_tuned_grid, "roc_auc")  

# Binds the best parameters the model
xgb_final <- finalize_workflow(xgb_wf, best_hyperparams)

xgb_final %>% 
  fit(data = train_set) %>% 
  pull_workflow_fit() %>% 
  vip(geom = "point")

# Fit the model on train and test on test set and come up with predictions
xgb_last_fit <- last_fit(xgb_final, train_split_obj)

xgb_last_fit  %>% 
  collect_predictions() %>% 
  roc_curve(Exited, .pred_0) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  
  # Uses default intercept = 0, slope = 1
  geom_abline(linetype = 2, alpha = 0.5,color = "gray50", size = 1.2)

