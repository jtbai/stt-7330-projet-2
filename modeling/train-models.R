# -------------------------------------------------------------------------
# Title: train-models
# Goal: Train models by finding optimal parameters through grid search and save models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


train_model <- function(models, data_train, path_models, nb_of_kfolds = 10){
  
  for (i in 1:3) {
    
    if (models[[i]]$ind_grid_search) {
      hyper_parameter <- models[[i]]$grid_search_params
      best_hyper_parameters <-  get_best_hyper_parameters(data_train, models[[i]]$model, nb_of_kfolds, hyper_parameter)
    } else {
      best_hyper_parameters <- NULL
    }
    
    model_function = get_model_function(models[[i]]$model)$model_function
    model_trained <- model_function(surface ~ ., data = data_train, control = best_hyper_parameters)
    
    saveRDS(object = model_trained, file = paste0(path_models, "model_",models[[i]]$model, ".rds"))
    
  }

}
