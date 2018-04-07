# -------------------------------------------------------------------------
# Title: feature-engineering-data-neural-net
# Goal: This script is aimed to functionalize the features engineering of the neural net models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

create_features_of_neural_net <- function(dt){
  
  # Replace NA's of set score by 0
  data_with_features_neural_net <- copy(dt)[is.na(winner_score_1), winner_score_1 := 0]
  data_with_features_neural_net[is.na(winner_score_2), winner_score_2 := 0]
  data_with_features_neural_net[is.na(winner_score_3), winner_score_3 := 0]
  data_with_features_neural_net[is.na(winner_score_4), winner_score_4 := 0]
  data_with_features_neural_net[is.na(winner_score_5), winner_score_5 := 0]
  
  data_with_features_neural_net[is.na(loser_score_1), loser_score_1 := 0]
  data_with_features_neural_net[is.na(loser_score_2), loser_score_2 := 0]
  data_with_features_neural_net[is.na(loser_score_3), loser_score_3 := 0]
  data_with_features_neural_net[is.na(loser_score_4), loser_score_4 := 0]
  data_with_features_neural_net[is.na(loser_score_5), loser_score_5 := 0]
  
  data_with_features_neural_net[, score := NULL]
  
  # One hot categorical vairables
  variables_to_one_hot <- c("winner_hand", "loser_hand", "winner_ioc", "loser_ioc")
  data_with_features_neural_net[, (variables_to_one_hot) := lapply(.SD, as.factor), .SDcol = variables_to_one_hot]
  data_with_features_neural_net <- one_hot(data_with_features_neural_net, cols = variables_to_one_hot)
  
  return(data_with_features_neural_net)
  
}
