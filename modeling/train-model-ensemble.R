# -------------------------------------------------------------------------
# Title: train-model-ensemble
# Goal: Fit ensemble model
# Date: April 2018
# Author: Christopher Blier-Wong
# -------------------------------------------------------------------------


# Define functions --------------------------------------------------------

getAccuracy <- function(predict_matrix, true_response) {
  
  true_matrix <- matrix(rep(true_response, ncol(predict_matrix)), nrow = length(true_response))
  true_predicted <- predict_matrix == true_matrix
  
  colMeans(true_predicted)
}

calculateVoterWeights <- function(predict_matrix, true_response, method) {
  
  if (method == "majority") {
    weights <- rep(1 / ncol(predict_matrix), ncol(predict_matrix))
  } else if (method == "accuracy") {
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
  
  apply(score_matrix, 1, which.max)
  
}

train_model_ensemble <- function(prediction_matrix, true_response, method, path_to_preds, index_to_train, index_to_test) {
  
  total_number_of_voters <- ncol(prediction_matrix)
  
  # Calculate weights
  current_weight <- calculateVoterWeights(prediction_matrix[index_to_train], true_response[index_to_train], method)
  predictions <- getVoter(prediction_matrix, current_weight)
  
  best_accuracy <- mean(predictions[index_to_test] == true_response[index_to_test])
  best_voters <- 1:total_number_of_voters
  
  # All combinations of voters
  for (i in 2:(total_number_of_voters - 1)) {
    voter_combinations <- combn(1:total_number_of_voters, i)
    
    for (j in 1:ncol(voter_combinations)) {
      
      current_voters <- voter_combinations[, j]
      
      current_predict_matrix <- as.matrix(prediction_matrix)[, current_voters]
      current_weight <- calculateVoterWeights(current_predict_matrix[index_to_train, ], true_response[index_to_train], method)
      current_predictions <- getVoter(current_predict_matrix, current_weight)
      current_accuracy <- mean(current_predictions[index_to_test] == true_response[index_to_test])

      if (current_accuracy > best_accuracy) {
        best_accuracy <- current_accuracy
        best_voters <- current_voters
      }
    }
  }
  
  # Unique voter
  if (max(getAccuracy(prediction_matrix[index_to_test], true_response[index_to_test]) > best_accuracy)) {
    best_accuracy <- max(getAccuracy(prediction_matrix[index_to_test, ], true_response[index_to_test]))
    best_voters <- which.max(getAccuracy(prediction_matrix[index_to_test, ], true_response[index_to_test]))
  }
  
  # Output best voters
  best_predict_matrix <- as.matrix(prediction_matrix)[, best_voters]
  best_weight <- calculateVoterWeights(best_predict_matrix[index_to_train | index_to_test, ], true_response[index_to_train | index_to_test], method)
  best_predictions <- getVoter(best_predict_matrix, best_weight)
  
  fwrite(data.frame(preds = best_predictions), file = paste0(path_to_preds, "preds_model_ensemble.csv"))
  print(best_voters)
}


