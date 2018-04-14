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

source("modeling/model-factory.R")

# Import data -------------------------------------------------------------

source("data/scripts/clean-data.R")
source("data/scripts/preprocess-data.R")

source("modeling/hyper-parameter-selection.R")

data_prepared <-  data_imputed
# Model -------------------------------------------------------------------

# Split train/test
test_index <- sample(1:nrow(data_prepared), 0.2 * nrow(data_prepared))
data_train <- data_prepared[-test_index,]
data_test <- data_prepared[test_index,]


# Naive bayes
model_bayes <-  naiveBayes(surface ~ ., data = data_train)
out <- predict(model_bayes, data_test)
sum(out == data_test[, surface]) / nrow(data_test)

# Tree-based
hyper_parameter <- list(minsplit = c(2, 5, 8, 10, 15, 20), maxdepth = c(1, 3, 5, 10))
best_hyper_parameters = get_best_hyper_parameters(data_train, "tree_based", 10, hyper_parameter)
model_function = get_model_function("tree_based")$model_function
model <- model_function(surface ~ ., data =data_train,   control = best_hyper_parameters)
out <- predict(model, data_test, type = "class")
sum(out == data_test[, surface]) / nrow(data_test)

plot(model, margin = 0.1)
text(model, use.n = T, cex = 0.8)

# LDA
model_lda <- lda(surface ~ ., data = data_train)
out <- predict(model_lda, data_test)$class
sum(out == data_test[, surface]) / nrow(data_test)

# QDA
model_qda <- qda(surface ~ ., data = data_train)
out <- predict(model_qda, data_test)$class
sum(out == data_test[, surface]) / nrow(data_test)

# Random Forrest
hyper_parameter <-   list(mtry = seq(4,16,4), ntree = c(700, 1000,2000))
best_hyper_parameters = get_best_hyper_parameters(data_train, "random_forest", 10, hyper_parameter)
model_function = get_model_function("random_forest")$model_function
model <- model_function(surface ~ ., data =data_train,   control = best_hyper_parameters)
out <- predict(model, data_test)
sum(out == data_test[, surface]) / nrow(data_test)
varImpPlot(model)

# SVM - gaussien
hyper_parameter <-   list(epsilon = c(0.1,0.01,0.001), cost = c(0.90, 0.95, 0.99, 1))
best_hyper_parameters = get_best_hyper_parameters(data_train, "svm_gaussien", 10, hyper_parameter)
model_function = get_model_function("svm_gaussien")$model_function
model <- model_function(surface ~ ., data =data_train, control= best_hyper_parameters)
out <- predict(model, data_test)
sum(out == data_test[, surface]) / nrow(data_test)

# SVM - polynomial
hyper_parameter <-   list(epsilon = c(0.1,0.01,0.001), cost = c(0.90, 0.95, 0.99, 1))
best_hyper_parameters = get_best_hyper_parameters(data_train, "svm_poly3", 10, hyper_parameter)
model_function = get_model_function("svm_poly3")$model_function
model <- model_function(surface ~ ., data =data_train, control= best_hyper_parameters)
out <- predict(model, data_test)
sum(out == data_test[, surface]) / nrow(data_test)


# Model logistic
model_multi <- multinom(surface ~ ., data = data_train)
out <- predict(model_multi, data_test)
sum(out == data_test[, surface]) / nrow(data_test)


# Hasard
props <- tabulate(as.numeric(data_test$surface), nbins = 3) / nrow(data_test)
1 - sum(props^2)
