# -------------------------------------------------------------------------
# Title: 
# Goal: Prepare data for modeling (FE and Imputations)
# Date: April 2018
# Author: Christopher Blier-Wong
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(mltools)
library(data.table)
library(tidyr)
library(keras)

# install_keras() # to install keras on local

# Import data -------------------------------------------------------------

data_imputed <- fread("data/data_modeling_neural_net.csv")

# Model -------------------------------------------------------------------

data_response <- as.integer(as.factor(data_imputed[, surface]))
data_response <- to_categorical(data_response)
data_train <- data_imputed[, -1, with = FALSE]

number_of_features <- ncol(data_train)

model_neural_net <- keras_model_sequential()

model_neural_net %>% 
  layer_dense(units = 60, activation = "relu", input_shape = c(number_of_features)) %>% 
  layer_dense(units = 4, activation = "softmax")

model_neural_net %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model_neural_net %>% fit(
  data_train, data_response, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history)


