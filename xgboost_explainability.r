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
library(DALEX)

library(SHAPforxgboost) # summary and dependency plot
library(shapviz) # waterfall and forceplot

# Read the data
churn_tbl <-  read.csv("data/Churn_Modelling.csv")

# Display the data
churn_tbl %>% View()

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

# Stratified test train split
set.seed(123)
train_split_obj <- initial_split(churn_features_tbl, prop = 0.80, strata = Exited)
train_set <- training(train_split_obj)
test_set <- testing(train_split_obj)

# Let's preprocess the data after train test split
churn_recipe <- 
  recipe(Exited ~ ., data = train_set) %>% 
  step_integer(all_nominal_predictors()) 

churn_prep <-  churn_recipe %>% prep(training = NULL)
churn_baked <- churn_prep %>% bake(new_data = NULL)

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
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(), #model complexity
  sample_size = tune(), 
  mtry = tune(),
  learn_rate = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")

xgb_wf <-  workflow() %>% 
  add_recipe(churn_recipe) %>% 
  add_model(xgb_spec)

k_folds <- vfold_cv(train_set, 5, strata = "Exited")

doParallel::registerDoParallel()

xgb_grid <-  grid_latin_hypercube(
  tree_depth(),
  learn_rate(),
  finalize(mtry(),train_set),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 30 # 30 hyperparameters
) 

xgb_tuned_grid <-  tune_grid(
  xgb_wf, 
  resamples = k_folds, 
  grid = xgb_grid,
  control = control_grid(save_pred = T)
)

# Binds the best parameters the model
xgb_final <- finalize_workflow(xgb_wf, select_best(xgb_tuned_grid, "roc_auc"))

# Fit the model on train and test on test set and come up with predictions
xgb_last_fit <- last_fit(xgb_final, train_split_obj)

# Precision recall
xgb_last_fit  %>% 
  collect_predictions() %>% 
  precision(Exited, .pred_class)

xgb_last_fit  %>% 
  collect_predictions() %>% 
  recall(Exited, .pred_class)


# Model explainability ----

# VIP
parsnip_fit_model <- extract_fit_parsnip(xgb_last_fit)
vip(parsnip_fit_model, geom = "point", num_features = 12)

churn_shap <-
  shap.prep(
    xgb_model = extract_fit_engine(xgb_last_fit),
    X_train = bake(churn_prep,
                   has_role("predictor"),
                   new_data = train_set,
                   composition = "matrix")
  )

shap.plot.summary(churn_shap)
# Most common SHAP viz(Global feature importance)
#          variable mean_abs_shap
#1:             Age    0.81480214
#2:   NumOfProducts    0.76543305
#3:  IsActiveMember    0.39534302
#4:          Gender    0.24777851
#5:         Balance    0.23067891
#6:       Geography    0.21708613
#7: EstimatedSalary    0.12725123
#8:     CreditScore    0.09397505
#9:          Tenure    0.06459320
#10:       HasCrCard  0.02512105

# When we plot by age, around 50 years of age, the plot branches. 
# This means there is another variable which is affecting.
# Trying out different color variables, we have narrowed down the 
# cause to isActiveMember
shap.plot.dependence(churn_shap, x = "Age", color = "age", 
                     alpha = 0.5, jitter_width = 0.1) 

shap.plot.dependence(churn_shap, x = "Age", color = "IsActiveMember", 
                     alpha = 0.5, jitter_width = 0.1) 

shap.viz <- shapviz(extract_fit_engine(xgb_last_fit), X_pred = bake(churn_prep,
                                                                    has_role("predictor"),
                                                                    new_data = train_set,
                                                                    composition = "matrix"), )


sv_force(shap.viz, row_id = 1)
sv_waterfall(shap.viz, row_id = 1)

# References:
# 1) https://www.r-bloggers.com/2023/01/shap-xgboost-tidymodels-love/
# 2) https://juliasilge.com/blog/board-games/
# 3) https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/
