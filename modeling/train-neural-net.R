# -------------------------------------------------------------------------
# Title: train-neural-net
# Goal: Train neural net
# Date: April 2018
# Author: Christopher Blier-Wong
# -------------------------------------------------------------------------

train_and_predict_neural_net <- function(model, data_train, data_test, path_to_model) {
  
  # Load packages
  library(keras)
  # install_keras() # to install keras on local
  
  # Create predictors and response variables
  x_train <- as.matrix(data_train[, -c("surface")])
  y_train <- to_categorical(data_train[, surface])[, -1]
  
  x_test <- as.matrix(data_test[, -c("surface")])
  y_test <- to_categorical(data_test[, surface])[, -1]

  # Define the model
  number_of_features <- ncol(x_train)
  
  model_neural_net <- keras_model_sequential()
  
  model_neural_net %>% 
    layer_dense(units = 25, activation = 'sigmoid', input_shape = c(number_of_features)) %>% 
    layer_dropout(rate = 0.1) %>% 
    layer_dense(units = 10, activation = 'sigmoid') %>%
    layer_dropout(rate = 0.05) %>%
    layer_dense(units = 3, activation = 'softmax')
  
  model_neural_net %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = c("accuracy")
  )
  
  # Fit the network
  model_neural_net %>% fit(
    x_train, y_train, 
    epochs = 200, batch_size = 128, 
    verbose = 1,
    validation_split = 0.2
  )
  
  save_model_hdf5(model_neural_net, paste0(path_to_model, "model_", model$model, ".hdf5"))
  
}
