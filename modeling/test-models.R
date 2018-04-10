# -------------------------------------------------------------------------
# Title: test-models
# Goal: Test few models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(e1071)
library(MASS)
library(nnet)
library(caret)
library(doParallel)


# Import data -------------------------------------------------------------

source("modeling/import-data-modeling.R")

# Import models -----------------------------------------------------------

source("modeling/model-factory.R")
source("modeling/hyper-parameter-selection.R")

# Run models --------------------------------------------------------------

# Naive bayes
model_bayes <-  naiveBayes(surface ~ ., data = data_modeling_classical_methods_train)
out <- predict(model_bayes, data_modeling_classical_methods_validation)
sum(out == data_modeling_classical_methods_validation[, surface]) / nrow(data_modeling_classical_methods_validation)

# Tree-based
hyper_parameter <- list(minsplit = c(2, 5, 10, 20), maxdepth = c(1, 3, 5, 8, 10))
best_hyper_parameters = get_best_hyper_parameters(data_modeling_classical_methods_train, "tree_based", 10, hyper_parameter)
model_function = get_model_function("tree_based")
model <- model_function$model_function(surface ~ ., data =data_modeling_classical_methods_train,   control = best_hyper_parameters)
out <- predict(model, data_modeling_classical_methods_validation)

model = get_model_function("tree_based")
model_tree <- model$model_function(surface ~ ., data = data_modeling_classical_methods_train, control = best_hyper_parameters)
out <- predict(model_tree, data_modeling_classical_methods_validation, type = "class")
sum(out == data_modeling_classical_methods_validation[, surface]) / nrow(data_modeling_classical_methods_validation)

plot(model, margin = 0.1)
text(model, use.n = T, cex = 0.8)

# LDA
model_lda <- lda(surface ~ ., data = data_modeling_classical_methods_train)
out <- predict(model_lda, data_modeling_classical_methods_validation)$class
sum(out == data_modeling_classical_methods_validation[, surface]) / nrow(data_modeling_classical_methods_validation)

# QDA
model_qda <- qda(surface ~ ., data = data_modeling_classical_methods_train)
out <- predict(model_qda, data_modeling_classical_methods_validation)$class
sum(out == data_modeling_classical_methods_validation[, surface]) / nrow(data_modeling_classical_methods_validation)

# Random Forrest
hyper_parameter <-   list(mtry = seq(4,16,4), ntree = c(700, 1000,2000))
best_hyper_parameters = get_best_hyper_parameters(data_modeling_classical_methods_train, "random_forest", 10, hyper_parameter)
model_function = get_model_function("random_forest")
model <- model_function$model_function(surface ~ ., data =data_modeling_classical_methods_train,   control = best_hyper_parameters)
out <- predict(model, data_modeling_classical_methods_validation)
sum(out == data_modeling_classical_methods_validation[, surface]) / nrow(data_modeling_classical_methods_validation)
varImpPlot(model_rf)

# SVM - gaussien
hyper_parameter <-   list(epsilon = c(0.1,0.01,0.001), cost = c(0.90, 0.95, 0.99, 1))
best_hyper_parameters = get_best_hyper_parameters(data_modeling_classical_methods_train, "svm_gaussien", 10, hyper_parameter)
model_function = model_function$model_function("svm_gaussien")
model <- model_function(surface ~ ., data =data_modeling_classical_methods_train,   control = best_hyper_parameters)
out <- predict(model, data_modeling_classical_methods_validation)

# SVM - polynomial
hyper_parameter <-   list(epsilon = c(0.1,0.01,0.001), cost = c(0.90, 0.95, 0.99, 1))
best_hyper_parameters = get_best_hyper_parameters(data_modeling_classical_methods_train, "svm_poly3", 10, hyper_parameter)
model_function = model_function$model_function("svm_poly3")
model <- model_function(surface ~ ., data =data_modeling_classical_methods_train,   control = best_hyper_parameters)
out <- predict(model, data_modeling_classical_methods_validation)


# Model logistic
model_multi <- multinom(surface ~ ., data = data_modeling_classical_methods_train)
out <- predict(model_multi, data_modeling_classical_methods_validation)
sum(out == data_modeling_classical_methods_validation[, surface]) / nrow(data_modeling_classical_methods_validation)


# Hasard
props <- tabulate(as.numeric(data_modeling_classical_methods_validation$surface), nbins = 3) / nrow(data_modeling_classical_methods_validation)
1 - sum(props^2)
