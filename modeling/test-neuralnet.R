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

# Train test split --------------------------------------------------------

data_response <- to_categorical(as.integer(as.factor(data_prepared[, surface])))[, -1]
data_features <- as.matrix(data_prepared[, -1, with = FALSE])

test_idx <- sample(1:nrow(data_features), floor(nrow(data_features) * 0.1), replace = FALSE)

X_train <- data_features[-test_idx, ]
X_test <- data_features[test_idx, ]

y_train <- data_response[-test_idx, ]
y_test <- data_response[test_idx, ]

# Model -------------------------------------------------------------------

number_of_features <- ncol(X_train)

model_neural_net <- keras_model_sequential()

model_neural_net %>% 
  layer_dense(units = 15, activation = 'sigmoid', input_shape = c(number_of_features)) %>% 
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
  epochs = 100, batch_size = 128, 
  verbose = 1,
  validation_split = 0.2
)

plot(history)

model_neural_net %>% evaluate(X_test, y_test)
