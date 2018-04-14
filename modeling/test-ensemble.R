# -------------------------------------------------------------------------
# Title: test-ensemble
# Goal: Fit ensemble model
# Date: April 2018
# Author: Christopher Blier-Wong
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(data.table)

# Import data -------------------------------------------------------------

data_prepared <- fread("data/data_modeling_neural_net.csv")
predict_nn <- fread("prediction/predict-nn.csv")

# Train test split --------------------------------------------------------

train_groups <- c(1)
test_groups <- c(2)

split123 <- data_prepared[, split_group]

predict_matrix <- cbind(predict_nn) # add more classifiers here
true_response <- data_prepared[split123 %in% train_groups, surface]

# Functions ---------------------------------------------------------------

calculateVoterWeights <- function(predict_matrix, true_response, method) {
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
  
  return(weights)
}

getVoter <- function(predict_matrix, weight) {
  
  weight_matrix <- matrix(rep(weights, each = nrow(predict_matrix)), nrow = nrow(predict_matrix), byrow = FALSE)
  
  score1 <- rowSums(weight_matrix * (predict_matrix == 1))
  score2 <- rowSums(weight_matrix * (predict_matrix == 2))
  score3 <- rowSums(weight_matrix * (predict_matrix == 3))
  score_matrix <- cbind(score1, score2, score3)
  
  ensemble_predict <- apply(score_matrix, 1, which.max)
}
