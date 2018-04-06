# -------------------------------------------------------------------------
# Title: test-models
# Goal: Test few models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(e1071)
library(randomForest)
library(rpart)
library(MASS)
library(nnet)

# Import data -------------------------------------------------------------

source("data/scripts/clean-data.R")
source("data/scripts/preprocess-data.R")

# Model -------------------------------------------------------------------

# Split train/test
test_index <- sample(1:nrow(data_imputed), 0.2 * nrow(data_imputed))
data_train <- data_imputed[-test_index,]
data_test <- data_imputed[test_index,]

# Naive bayes
model_bayes <-  naiveBayes(surface ~ ., data = data_train)
out <- predict(model_bayes, data_test)
sum(out == data_test[, surface]) / nrow(data_test)

# Tree-based
model_tree <- rpart(surface ~ ., data = data_train)
out <- predict(model_tree, data_test, type = "class")
sum(out == data_test[, surface]) / nrow(data_test)

plot(model_tree, margin = 0.1)
text(model_tree, use.n = T, cex = 0.8)

# LDA
model_lda <- lda(surface ~ ., data = data_train)
out <- predict(model_lda, data_test)$class
sum(out == data_test[, surface]) / nrow(data_test)

# QDA
model_qda <- qda(surface ~ ., data = data_train)
out <- predict(model_qda, data_test)$class
sum(out == data_test[, surface]) / nrow(data_test)

# Random Forrest
model_rf <- randomForest(surface ~ ., data = data_train)
out <- predict(model_rf, data_test)
sum(out == data_test[, surface]) / nrow(data_test)
varImpPlot(model_rf)

# Model logistic
model_multi <- multinom(surface ~ ., data = data_train)
out <- predict(model_multi, data_test)
sum(out == data_test[, surface]) / nrow(data_test)

# Hasard
props <- tabulate(as.numeric(data_test$surface), nbins = 3) / nrow(data_test)
1 - sum(props^2)
