# -------------------------------------------------------------------------
# Title: output_predict.R
# Goal: Make predictions for each model
# Date: April 2018
# Author: Sofia Harrouch/Stéphane Caron
# -------------------------------------------------------------------------

predict_models <- function(models, new_data, path_models, path_preds) {
  
  for (i in 1:seq_along(models)) {
    
    model <- readRDS(paste0(path_models, "model_", models[[i]]$model, ".rds"))

    if (get_model_function(models[[i]]$model)$ind_list_output){
      predictions <- predict(model, new_data)[[get_model_function(models[[i]]$model)$prediction_type]] 
    } else {
      predictions <- predict(model, new_data, type = get_model_function(models[[i]]$model)$prediction_type) 
    }
    
    fwrite(data.frame(preds = predictions), file = paste0(path_preds, "preds_model_", models[[i]]$model, ".csv"))
    
  }
  
}
