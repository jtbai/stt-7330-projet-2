# -------------------------------------------------------------------------
# Title: test-ensemble
# Goal: Fit ensemble model
# Date: April 2018
# Author: Christopher Blier-Wong
# -------------------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(data.table)

# Import data -------------------------------------------------------------

data_prepared <- fread("data/data_modeling_classical_methods.csv")

predictions_bayes <- fread("data/predictions/preds_model_bayes.csv")
predictions_lda <- fread("data/predictions/preds_model_lda.csv")
predictions_multinomial <- fread("data/predictions/preds_model_multinomial.csv")
# predictions_neuralnet <- fread("data/predictions/preds_model_neuralnet.csv")
predictions_qda <- fread("data/predictions/preds_model_qda.csv")
predictions_random_forest <- fread("data/predictions/preds_model_random_forest.csv")
predictions_svm_gaussien <- fread("data/predictions/preds_model_svm_gaussien.csv")
predictions_svm_poly3 <- fread("data/predictions/preds_model_svm_poly3.csv")
predictions_tree_based <- fread("data/predictions/preds_model_tree_based.csv")

# Train test split --------------------------------------------------------

train_groups <- c(1)
test_groups <- c(2)

split123 <- data_prepared[, split_group]

predict_matrix <- cbind(predictions_bayes,
                        predictions_lda,
                        predictions_multinomial,
                        # predictions_neuralnet,
                        predictions_qda,
                        predictions_random_forest,
                        predictions_svm_gaussien,
                        predictions_svm_poly3,
                        predictions_tree_based) # add more classifiers here

true_response <- data_prepared[split123 %in% test_groups, surface]

# Functions ---------------------------------------------------------------

getAccuracy <- function(predict_matrix, true_response) {
  true_matrix <- matrix(rep(true_response, ncol(predict_matrix)), nrow = length(true_response))
  true_predicted <- predict_matrix == true_matrix
  accuracy <- colMeans(true_predicted)
  
  return(accuracy)
}

calculateVoterWeights <- function(predict_matrix, true_response, method) {
  if(method == "majority") {
    weights <- rep(1 / ncol(predict_matrix), ncol(predict_matrix))
  } else if(method == "accuracy") {
    weights <- getAccuracy(predict_matrix, true_response)
    weights <- weights / sum(weights)
  } else {
    print("Wrong weight method. Must be majority or accuracy")
  }
  
  return(weights)
}

getVoter <- function(predict_matrix, weight) {
  
  weight_matrix <- matrix(rep(weight, each = nrow(predict_matrix)), nrow = nrow(predict_matrix), byrow = FALSE)
  
  score1 <- rowSums(weight_matrix * (predict_matrix == 1))
  score2 <- rowSums(weight_matrix * (predict_matrix == 2))
  score3 <- rowSums(weight_matrix * (predict_matrix == 3))
  score_matrix <- cbind(score1, score2, score3)
  
  ensemble_predict <- apply(score_matrix, 1, which.max)
  
  return(ensemble_predict)
}

# Overall voter -----------------------------------------------------------

weight_type <- "accuracy"

total_number_of_voters <- ncol(predict_matrix)

current_weight <- calculateVoterWeights(predict_matrix, true_response, weight_type)
predictions <- getVoter(predict_matrix, current_weight)

best_accuracy <- mean(predictions == true_response)
best_voters <- 1:total_number_of_voters

# All combinations of voters ----------------------------------------------

for(i in 2:(total_number_of_voters - 1)) {
  voter_combinations <- combn(1:total_number_of_voters, i)
  
  for(j in 1:ncol(voter_combinations)) {
    
    current_voters <- voter_combinations[, j]
    
    current_predict_matrix <- as.matrix(predict_matrix)[, current_voters]
    current_weight <- calculateVoterWeights(current_predict_matrix, true_response, weight_type)
    current_predictions <- getVoter(current_predict_matrix, current_weight)
    current_accuracy <- mean(current_predictions == true_response)
    if(current_accuracy > best_accuracy) {
      best_accuracy <- current_accuracy
      best_voters <- current_voters
    }
  }
}


# Unique voters -----------------------------------------------------------

if(max(getAccuracy(predict_matrix, true_response) > best_accuracy)) {
  best_accuracy <- max(getAccuracy(predict_matrix, true_response))
  best_voters <- which.max(getAccuracy(predict_matrix, true_response))
}

# Output best voters ------------------------------------------------------

best_predict_matrix <- as.matrix(predict_matrix)[, best_voters]
best_weight <- calculateVoterWeights(best_predict_matrix, true_response, weight_type)
best_predictions <- getVoter(best_predict_matrix, best_weight)

fwrite(data.frame(preds = best_predictions), file = "data/predictions/preds_model_ensemble.csv")
