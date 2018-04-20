# -------------------------------------------------------------------------
# Title: performance_measures.R
# Goal: This script is aimed to calculate different measures of classifiers's performance
# Date: April 2018
# Author: Sofia HARROUCH
# -------------------------------------------------------------------------

get_proportions <- function(classe, model_name, prediction_matrix){

  model_prediction <- prediction_matrix[[model_name]]

  TN <- sum((prediction_matrix$true_response == classe) & (model_prediction == classe))
  TP <- sum((prediction_matrix$true_response != classe) & (model_prediction != classe))
  FN <- sum((prediction_matrix$true_response != classe) & (model_prediction == classe))
  FP <- sum((prediction_matrix$true_response == classe) & (model_prediction != classe))
  
  proportion <- c(TN, TP, FN, FP)
  
  return(proportion)
}


get_performance <- function(model.preds, prediction_matrix){

  proportion <- matrix(NA,3,4)
  proportion[1,] <-  get_proportions(classe= 1, model_name = model.preds, prediction_matrix)
  proportion[2,] <-  get_proportions(classe= 2, model_name = model.preds, prediction_matrix)
  proportion[3,] <-  get_proportions(classe= 3, model_name = model.preds, prediction_matrix)
  
  average_accuracy <- sum((proportion[,1]+proportion[,2])/ (proportion[,1] + proportion[,2]+ proportion[,3]+proportion[,4])) / 3
  error_rate <- sum((proportion[,3]+proportion[,4])/ (proportion[,1] + proportion[,2]+ proportion[,3]+proportion[,4])) /3
  precision_u <- sum(proportion[,2])/ sum(proportion[,2] + proportion[,4])
  recall_u <- sum(proportion[,2])/ sum(proportion[,2] + proportion[,3])
  Fscore_u <- 2*(precision_u * recall_u)/ (precision_u + recall_u)
  precision_m <- (sum(proportion[,2])/ sum(proportion[,2] + proportion[,4]))/3
  recall_m <- (sum(proportion[,2])/ sum(proportion[,2] + proportion[,3]))/3
  Fscore_m <- 2*(precision_m * recall_m)/ (precision_m + recall_m)
  
  measures_names <- c("average_accuracy", "error_rate", "precision_u","recall_u","Fscore_u","precision_m","recall_m","Fscore_m")
  measures_performance <- c(average_accuracy, error_rate, precision_u,recall_u,Fscore_u,precision_m,recall_m,Fscore_m)
  performance <- data.frame(measures_names,measures_performance)
  
  return(performance)
  }



