# -------------------------------------------------------------------------
# Title: preds_rapport.R
# Goal: prediction des modeles pour le rapport
# Date: April 2018
# Author: Sofia Harrouch/Stéphane Caron
# -------------------------------------------------------------------------

predict_models_rapport <- function(models,true_response,list_index, path_rapport, path_preds) {
  
  list_preds <- lapply(models, function(model){
    fread(file = paste0(path_preds, "preds_model_", model$model, ".csv"))
  })
  data_preds <- data.table(do.call(cbind, list_preds))
  
  data_preds[list_index$index_train, split := "train"]
  data_preds[list_index$index_test, split := "test"]
  data_preds[list_index$index_validation, split := "validation"]
  data_preds[,true_response:= true_response]
  
  fwrite(data_preds, paste0(path_rapport, "prediction_rapport.csv"))
}

