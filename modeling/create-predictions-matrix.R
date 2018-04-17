# -------------------------------------------------------------------------
# Title: create-predictions-matrix
# Goal: Import predictions and create a matrix for different selected models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

create_predictions_matrix <- function(path_to_preds, models, index) {
  
  predictions <- lapply(seq_along(models), function(model) {
    models[[model]]$model = fread(paste0(path_to_preds, "preds_model_", models[[model]]$model, ".csv"))
  })
  
  predictions_matrix <- do.call(cbind, predictions)
  names(predictions_matrix) <- sapply(seq_along(models), function(model){
    models[[model]]$model
  })
  
  return(predictions_matrix[index, ])
  
}