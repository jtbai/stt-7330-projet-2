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

train_groups <- c(1)
test_groups <- c(2)

split123 <- data_prepared[, split_group]

data_response <- to_categorical(data_prepared[, surface])[, -1]
data_features <- as.matrix(data_prepared[, -c("surface", "split_group")])

X_train <- data_features[split123 %in% train_groups, ]
X_test <- data_features[split123 %in% test_groups, ]

y_train <- data_response[split123 %in% train_groups, ]
y_test <- data_response[split123 %in% test_groups, ]

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

predict_proba <- model_neural_net %>% predict(data_features)
predict_class <- apply(predict_proba, 1, which.max)

write.csv(predict_proba, "prediction/predict-proba-nn.csv")
write.csv(class, "prediction/predict-nn.csv")
