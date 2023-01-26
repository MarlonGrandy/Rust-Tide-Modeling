#This is the first test of training a model to predict rust tide occurrences on the Narragansett Long Term Time Series. 
library(tidyverse)
library(tidymodels)
library(themis)
library(vip)
library(unpivotr)

set.seed(15)

#factoring count and lagging vars
data <- data_comb |>
  tidytable::separate.(id, into = c("year", "week"), sep = "_") |>
  dplyr:: select(-"year") |>
  mutate(WSpdnolag = WSpd, flownolag = flow_mean) |>
  mutate(count = as.factor(
    ifelse(`count` >= 20000, 'high', "low" )))|>
  mutate_at(c("count", "week"),lead, n = 1) |>
  mutate_at(c("Surface Temp"), lag, n = 1) |>
  mutate_at(c("TotPAR"), lag, n = 3) |>
  mutate_at(c("flow_mean"), lag, n = 4) |>
  mutate_at(c("WSpd"), lag, n = 2) |>
  mutate_at(c("Wdir"), lag, n = 2) 
  
#randomly ordering data
data <-  data[sample(1:nrow(data)), ]


#splitting training and testing data sets
set.seed(15)
split <- initial_time_split(data, prop = 8/10, lag = 0)
train_raw <- training(split)
test_raw <- testing(split)


#creating training and testing sets
test <- test_raw |>
  dplyr::select( "count", "Surface Temp","flow_mean" , "Wdir",  "TotPAR", "flownolag") |>
  tidyr::drop_na()
train <- train_raw |>
  dplyr::select( "count" ,"Surface Temp",  "flow_mean" ,"Wdir", "TotPAR", "flownolag") |>
  tidyr::drop_na()


#preprocessing recipe
rust_recipe <- train |> recipe(count ~ ., data = train)  |>
 step_log(TotPAR, signed = TRUE)|>
  step_log(flow_mean)|>
  step_log(flownolag)|>
 #step_log(prcp_cum, signed = TRUE)|>
  step_BoxCox() |>
  step_normalize(all_numeric_predictors())











#models to try
mlnn <- mlp(mode = "classification", dropout = .1, engine = "nnet", hidden_units = 2, epochs = 250)
rf <- rand_forest(mode = "classification", engine = "ranger", trees = 1500, min_n = 40) 
bag <- bag_tree(
  mode = "unknown",
  cost_complexity = 0,
  tree_depth = NULL,
  min_n = 2,
  class_cost = NULL,
  engine = "rpart"
)



#creating resample folds and model validation metrics
resamples = vfold_cv(train, v = 5, strata = count)
# ratio between all the instances that were correctly classified in the positive class 
#against the total number of instances classified in the positive class
model_metrics <- yardstick::metric_set( yardstick::accuracy,  yardstick::sensitivity,  yardstick::specificity, 
                                        yardstick::precision,  yardstick::recall,  yardstick::f_meas)


#creating a recipe and workflow
set.seed(15)
bal_rec <- rust_recipe |>
  step_dummy(all_nominal_predictors()) |>
  step_smote(count, over_ratio = .5) 

bal_wf <-
  workflow() |>
  add_recipe(bal_rec) |>
  add_model(mlnn)



#validating with resamples
bal_rs <- 
  bal_wf |>
  fit_resamples(
    resamples = resamples,
    control = control_resamples(save_pred = TRUE)
  ) 

#visualizing resamples with metrics and confusion matrix
rs_confusionmatrix <- conf_mat_resampled(bal_rs)
rs_metrics <-   fit_resamples(bal_wf,
                              resamples = resamples, metrics = model_metrics) |>
  collect_metrics()




#testing data
fit <- fit(bal_wf, data = train)

predict(fit, test)
pred <- augment(fit, test)

dat_met <- metrics(pred, truth = `count`, estimate=.pred_class)



#plotting a confusion matrix
confusion_matrix_test <- conf_mat(pred, count, .pred_class)
plt <- as.data.frame(confusion_matrix_test$table)
confusion <- ggplot(plt, aes(Prediction,Truth, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Truth",y = "Prediction") +
  scale_x_discrete(labels=c("High","Low")) +
  scale_y_discrete(labels=c("High","Low"))



vars <- fit |>
  extract_fit_parsnip() |>
  vip()






