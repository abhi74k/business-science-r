library(tidyverse)
library(tidymodels)
library(tidyquant)
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
library(SHAPforxgboost) # Fast implement for XGBoost
library(umap)
library(mclust)

# Supervised clustering 
# Building a tuned XGBoost classification model with Exited as the class variable
# Get the SHAP score
# Get the SHAP score only for Exited = 0
# Cluster using GMM
# Visualize in 2D using UMAP and color it with clusters from GMM

# Read the data
churn_tbl <-  read.csv("data/Churn_Modelling.csv")

churn_features_tbl <- churn_tbl %>% 
  select(-RowNumber, -CustomerId, -Surname) %>% 
  mutate(
    Exited = Exited %>% as.factor()
  )

train_set <- churn_features_tbl

# Let's preprocess the data after train test split
churn_recipe <- 
  recipe(Exited ~ ., data = train_set) %>% 
  step_integer(all_nominal_predictors()) 

churn_prep <-  churn_recipe %>% prep(training = NULL)
churn_baked <- churn_prep %>% bake(new_data = NULL)

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

xgb_fitted <- xgb_final %>% 
  fit(data = train_set) #Sets the 

churn_shap <-
  shap.prep(
    xgb_model = extract_fit_engine(xgb_fitted),
    X_train = bake(churn_prep,
                   has_role("predictor"),
                   new_data = train_set,
                   composition = "matrix")
  )

shap_values <- shap.values(
  extract_fit_engine(xgb_fitted), 
  X_train = bake(churn_prep,
                 has_role("predictor"),
                 new_data = train_set,
                 composition = "matrix"))

shap_score <- shap_values$shap_score %>% mutate(shap = T)

churn_transformed_tbl <- shap_score

churn_transformed_tbl %>% View()

churn_transformed_predictors <- churn_tbl %>% 
  select(-RowNumber, -CustomerId, -Surname) %>% 
  mutate(
    Exited = Exited %>% as.factor()
  ) %>% 
  mutate(
    Geography = as.numeric(as.factor(Geography)),
    Gender = as.numeric(as.factor(Gender)),
    Exited = as.numeric(as.factor(Exited)),
    shap = F
  ) %>% 
  select(-Exited)

final_dataset <- shap_score %>% 
  cbind(churn_tbl %>% select(Exited)) %>% 
  filter(Exited == 0) %>% 
  select(-shap, -Exited)

umap_model <- final_dataset %>% umap()

umap_result <-
  umap_model$layout %>% 
  as_tibble() %>% 
  set_names(c("x", "y"))

# Resources
# GMM
# https://vitalflux.com/gaussian-mixture-models-what-are-they-when-to-use/

gmm <- Mclust(final_dataset)
gmm$classification

gmm_result <- umap_result %>% 
  cbind(gmm$classification) %>% 
  set_names(c("x", "y", "cluster")) %>% 
  mutate(
    cluster = as.factor(cluster) # Required to covert to discrete scale for plotting
  )

gmm_result %>% 
  ggplot(aes(x = x, y = y, color=cluster)) +
  geom_point() + 
  scale_color_viridis_d()
