# -------------------------------------------------------------------------
# Title: create-models
# Goal: This script aims to create/train/predict our different models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(e1071)
library(MASS)
library(mltools)
library(tidyr)
library(nnet)
library(data.table)
library(rpart)
library(jsonlite)
library(stringr)
library(purrr)
library(dplyr)
library(randomForest)


# Source functions --------------------------------------------------------

source("modeling/model-factory.R")
source("modeling/hyper-parameter-selection.R")
source("modeling/import-data-modeling.R")
source("modeling/train-models.R")
source("modeling/output_predict.R")
source("modeling/create-predictions-matrix.R")
source("modeling/train-model-ensemble.R")
source("modeling/preds_rapport.R")

# Define global configuration ---------------------------------------------

# Import models inputs
model_inputs <- fromJSON("modeling/inputs/input_models.json")

# Do grid search or not
ind_train_model <- FALSE

train_groups <- 1L
test_groups <- 2L
validation_groups <- 3L


# Create models -----------------------------------------------------------

# Look in input_file if include neural net or not
input_neural_net <- which(unlist(lapply(seq_along(model_inputs), function(x){model_inputs[[x]]$model})) == "neural_net")
if (length(input_neural_net) != 0) {
  data_train_nn <- import_data_modeling("data/data_modeling_neural_net.csv", selected_group = train_groups)
  data_test_nn <- import_data_modeling("data/data_modeling_neural_net.csv", selected_group = c(train_groups, test_groups, validation_groups))
  if (ind_train_model) {
    source("modeling/train-neural-net.R")
    train_and_predict_neural_net(model_inputs[[input_neural_net]], data_train_nn, "modeling/models//")
  }
  source("modeling/predict-neural-net.R")
  predict_neural_net(model_inputs[[input_neural_net]], data_test_nn, "modeling/models/",  "data/predictions/")
  model_inputs_classical <- model_inputs[-input_neural_net]
} else {
  model_inputs_classical <- model_inputs
}

# Import datasets
data_train <- import_data_modeling("data/data_modeling_classical_methods.csv", selected_group = train_groups)
data_test <- import_data_modeling("data/data_modeling_classical_methods.csv", selected_group = c(train_groups, test_groups, validation_groups))

index_train <- fread("data/data_modeling_classical_methods.csv")$split_group == train_groups
index_test <- fread("data/data_modeling_classical_methods.csv")$split_group == test_groups
index_validation <- fread("data/data_modeling_classical_methods.csv")$split_group == validation_groups

# Train models and verify none are missing
input_model_ensemble <- which(unlist(lapply(seq_along(model_inputs_classical), function(x){model_inputs_classical[[x]]$model})) == "ensemble")
if (length(input_model_ensemble) != 0) {
  model_inputs_classical <- model_inputs_classical[-input_model_ensemble]
}
if (ind_train_model) {
  train_model(model_inputs_classical, data_train, "modeling/models/", 10)
} else {
  actual_models <- list.files("modeling/models/")
  input_model_ensemble <- which(unlist(lapply(seq_along(model_inputs), function(x){model_inputs[[x]]$model})) == "ensemble")
  modeles_to_verify <- paste0("model_", as.character(unlist(lapply(model_inputs[-input_model_ensemble], function(x){x$model}))))
  test <- sapply(modeles_to_verify, function(model) {
    any(str_detect(actual_models, model))
  })
  
  if (any(test == 0)) {
    stop(paste(c("Some required models are missing from your model folder. Those missing trained models are: ", paste(names(which(test == 0)), collapse = ", "))))
  }
}

# Make predictions on test set
predict_models(model_inputs_classical, new_data = data_test, path_models = "modeling/models/", path_preds = "data/predictions/")

# Create prediction matrix
predict_matrix <- create_predictions_matrix("data/predictions/", model_inputs_classical)

# Run ensemble model
if (length(input_model_ensemble) != 0) {
  true_response <- import_data_modeling("data/data_modeling_classical_methods.csv", selected_group = c(train_groups, test_groups, validation_groups))$surface
  train_model_ensemble(predict_matrix, true_response, "accuracy", "data/predictions/", index_train, index_test)
}

predict_models_rapport(models = model_inputs, list_index = list(index_train = index_train, index_test = index_test, index_validation, index_validation), path_rapport ="" , path_preds="data/predictions/")

