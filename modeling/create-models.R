# -------------------------------------------------------------------------
# Title: create-models
# Goal: This script aims to create/train/predict our different models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(e1071)
library(MASS)
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


# Define global configuration ---------------------------------------------

# Import models inputs
model_inputs <- fromJSON("modeling/inputs/input_models.json")

# Do grid search or not
ind_train_model <- FALSE

train_groups <- 1L
test_groups <- 2L


# Create models -----------------------------------------------------------

# Import datasets
data_train <- import_data_modeling("data/data_modeling_classical_methods.csv", selected_group = train_groups)
data_test <- import_data_modeling("data/data_modeling_classical_methods.csv", selected_group = test_groups)

# Train models and verify none are missing
if (ind_train_model) {
  train_model(model_inputs, data_train, "modeling/models/", 10)
} else {
  actual_models <- list.files("modeling/models/")
  modeles_to_verify <- paste0("model_", as.character(unlist(lapply(model_inputs, function(x){x$model}))))
  test <- sapply(modeles_to_verify, function(model) {
    any(str_detect(actual_models, model))
  })
  
  if (any(test == 0)) {
    stop(paste(c("Some required models are missing from your model folder. Those missing trained models are: ", paste(names(which(test == 0)), collapse = ", "))))
  }
}

# Make predictions on test set
predict_models(model_inputs, new_data = data_test, path_models = "modeling/models/", path_preds = "data/predictions/")

# Create prediction matrix
predict_matrix <- create_predictions_matrix("data/predictions/", model_inputs)

# Run ensemble model
true_response_test <- import_data_modeling("data/data_modeling_classical_methods.csv", selected_group = test_groups)$surface




