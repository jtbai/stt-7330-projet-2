# -------------------------------------------------------------------------
# Title: train-models
# Goal: This script takes optimal parameters from grid search as input and train models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(e1071)
library(MASS)
library(nnet)
library(data.table)
library(rpart)


# Import parameters -------------------------------------------------------




# Train models ------------------------------------------------------------



source("modeling/model-factory.R")
source("modeling/hyper-parameter-selection.R")

# Import data -------------------------------------------------------------

data_prepared <- fread("data/data_modeling_classical_methods.csv")
data_prepared[, surface := as.factor(surface)]


# Split train/test --------------------------------------------------------

train_groups <- 1L
test_groups <- 2L

data_train <- data_prepared[split_group %in% train_groups, -(c("split_group")), with = FALSE]
data_test <- data_prepared[split_group %in% test_groups, -(c("split_group")), with = FALSE]


# Search optimal models ---------------------------------------------------


# Naive bayes
model_bayes <-  naiveBayes(surface ~ ., data = data_train)

# Tree-based
model_function = get_model_function("tree_based")$model_function
model_tree <- model_function(surface ~ ., data = data_train, control = best_hyper_parameters_tree)

# LDA
model_lda <- lda(surface ~ ., data = data_train)

# QDA
model_qda <- qda(surface ~ ., data = data_train)

# Random Forrest
model_function = get_model_function("random_forest")$model_function
model_rf <- model_function(surface ~ ., data =data_train,   control = best_hyper_parameters_rf)

# SVM - gaussien
model_function = get_model_function("svm_gaussien")$model_function
model_svm_gaussien <- model_function(surface ~ ., data =data_train, control= best_hyper_parameters_svm_gaussien)

# SVM - polynomial
model_function = get_model_function("svm_poly3")$model_function
model_svm_poly3 <- model_function(surface ~ ., data =data_train, control= best_hyper_parameters_svm_poly3)

# Model logistic
model_multi <- multinom(surface ~ ., data = data_train)
