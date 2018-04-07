# -------------------------------------------------------------------------
# Title: test-ensemble
# Goal: Fit ensemble model
# Date: April 2018
# Author: Christopher Blier-Wong
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(e1071)
library(randomForest)
library(rpart)
library(MASS)
library(nnet)

# Import data -------------------------------------------------------------

data_prepared <- fread("data/data_modeling_neural_net.csv")

predict_matrix <- matrix(sample(1:3, 5 * 1000, replace = TRUE), nrow = 1000, ncol = 5)
true_response <- sample(1:3, 1000, replace = TRUE)

# Functions ---------------------------------------------------------------

getVoter <- function(predict_matrix, true_response, method) {
  
  if(method == "majority") {
    weights <- rep(1 / ncol(predict_matrix), ncol(predict_matrix))
  } else if(method == "accuracy") {
    true_matrix <- matrix(rep(true_response, ncol(predict_matrix)), nrow = length(true_response))
    true_predicted <- predict_matrix == true_matrix
    weights <- colMeans(true_predicted)
    weights <- model_accuracy / sum(model_accuracy)
  } else {
    print("Wrong weight method. Must be majority or accuracy")
  }
  
  weight_matrix <- matrix(rep(weights, each = nrow(predict_matrix)), nrow = nrow(predict_matrix), byrow = FALSE)
  
  score1 <- rowSums(weight_matrix * (predict_matrix == 1))
  score2 <- rowSums(weight_matrix * (predict_matrix == 2))
  score3 <- rowSums(weight_matrix * (predict_matrix == 3))
  score_matrix <- cbind(score1, score2, score3)
  
  ensemble_predict <- apply(score_matrix, 1, which.max)
}

