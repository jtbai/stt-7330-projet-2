# -------------------------------------------------------------------------
# Title: predict-neural-net
# Goal: Make predictions for neural net model
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

predict_neural_net <- function(model, new_data, path_to_model, path_to_preds) {
  
  library(keras)
  # install_keras
  
  x_test <- as.matrix(new_data[, -c("surface")])
  
  model_neural_net <- load_model_hdf5(paste0(path_to_model, "model_", model$model, ".hdf5"))

  # Predicts
  predict_proba <- model_neural_net %>% predict(x_test)
  predictions <- apply(predict_proba, 1, which.max)

  fwrite(data.frame(preds = predictions), file = paste0(path_to_preds, "preds_model_", model$model,".csv"))
  
}
