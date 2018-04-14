# -------------------------------------------------------------------------
# Title: test-neuralnet
# Goal: Train neural net
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

data_prepared <- fread("data/data_modeling_neural_net.csv")
data_prepared[, surface := as.factor(surface)]

# Model -------------------------------------------------------------------

# Split train/test

X_train <- as.matrix(data_prepared[split_group == 1, -(c("split_group", "surface")), with = FALSE])
X_test <- as.matrix(data_prepared[split_group == 2, -(c("split_group", "surface")), with = FALSE])

y_train <- to_categorical(as.matrix(data_prepared[split_group == 1, c("surface"), with = FALSE]))[, -1]
y_test <- to_categorical(as.matrix(data_prepared[split_group == 2, c("surface"), with = FALSE]))[, -1]

# Model -------------------------------------------------------------------

number_of_features <- ncol(X_train)

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

history <- model_neural_net %>% fit(
  X_train, y_train, 
  epochs = 200, batch_size = 128, 
  verbose = 1,
  validation_split = 0.2
)

predict_proba_all <- model_neural_net %>% predict(as.matrix(data_prepared[, -(c("split_group", "surface")), with = FALSE]))

predict_neural_network <- apply(predict_proba_all, 1, which.max)