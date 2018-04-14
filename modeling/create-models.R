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

# Source functions --------------------------------------------------------

source("modeling/model-factory.R")
source("modeling/hyper-parameter-selection.R")
source("modeling/import-data-modeling.R")
source("modeling/grid-search.R")


# Define global configuration ---------------------------------------------

# Do grid search or not
ind_train_model <- FALSE

train_groups <- 1L
test_groups <- 2L


# Create models -----------------------------------------------------------

data_train <- import_data_modeling("data/data_modeling_classical_methods.csv", ind_train = train_groups, ind_test = test_groups)$data_train
data_test <- import_data_modeling("data/data_modeling_classical_methods.csv", ind_train = train_groups, ind_test = test_groups)$data_test

if (ind_train_model) {
  train_model(data_train, data_test, path = "modeling/models/")
} else {
  actual_models <- list.files("modeling/models/")
  modeles_to_verify <- c("model_bayes", "model_lda", "model_qda", "model_tree", "model_rf", "model_svm_gaussien", "model_svm_poly3", "model_multi")
  test <- sapply(modeles_to_verify, function(model) {
    any(str_detect(actual_models, model))
  })
  
  if (any(test == 0)) {
    stop(paste(c("Some required models are missing from your model folder. Those missing trained models are: ", paste(names(which(test == 0)), collapse = ", "))))
  }
}




