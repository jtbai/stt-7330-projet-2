# -------------------------------------------------------------------------
# Title: performance_measures.R
# Goal: This script is aimed to calculate different measures of classifiers's performance
# Date: April 2018
# Author: Sofia HARROUCH
# -------------------------------------------------------------------------

prediction <- fread("prediction_rapport.csv")


get_proportions <- function(classe, model_name = c("bayes.preds","lda.preds","qda.preds","tree_based.preds","random_forest.preds","svm_gaussien.preds","svm_ploy3.preds","multinomial.preds","ensemble.preds")){
  model_name <- match.arg(model_name)
  model_prediction <- eval(parse(text=paste0("prediction$",model_name)))

  TN <- sum((prediction$true_response == classe) & (model_prediction == classe))
  TP <- sum((prediction$true_response != classe) & (model_prediction != classe))
  FN <- sum((prediction$true_response != classe) & (model_prediction == classe))
  FP <- sum((prediction$true_response == classe) & (model_prediction != classe))
  
  proportion <- c(TN, TP, FN, FP)
  
  return(proportion)
}



get_performance <- function(model.preds ){

  proportion <- matrix(NA,3,4)
  proportion[1,] <-  get_proportions(classe= 1, model_name = model.preds)
  proportion[2,] <-  get_proportions(classe= 2, model_name = model.preds)
  proportion[3,] <-  get_proportions(classe= 3, model_name = model.preds)
  
  average_accuracy <- sum((proportion[,1]+proportion[,2])/ (proportion[,1] + proportion[,2]+ proportion[,3]+proportion[,4])) / 3
  error_rate <- sum((proportion[,3]+proportion[,4])/ (proportion[,1] + proportion[,2]+ proportion[,3]+proportion[,4])) /3
  precision_u <- sum(proportion[,2])/ sum(proportion[,2] + proportion[,4])
  recall_u <- sum(proportion[,2])/ sum(proportion[,2] + proportion[,3])
  Fscore_u <- (precision_u * recall_u)/ (precision_u + recall_u)
  precision_m <- (sum(proportion[,2])/ sum(proportion[,2] + proportion[,4]))/3
  recall_m <- (sum(proportion[,2])/ sum(proportion[,2] + proportion[,3]))/3
  Fscore_m <- (precision_m * recall_m)/ (precision_m + recall_m)
  
  measures_names <- c("average_accuracy", "error_rate", "precision_u","recall_u","Fscore_u","precision_m","recall_m","Fscore_m")
  measures_performance <- c(average_accuracy, error_rate, precision_u,recall_u,Fscore_u,precision_m,recall_m,Fscore_m)
  performance <- data.frame(measures_names,measures_performance)
  
  return(performance)
  }


performance1 <- get_performance(model.preds = "bayes.preds")

  
model_name <- list("bayes","lda","qda","tree_based","random_forest","svm_gaussien","svm_ploy3","multinomial","ensemble")
models <- c("bayes.preds","lda.preds","qda.preds","tree_based.preds","random_forest.preds","svm_gaussien.preds","svm_ploy3.preds","multinomial.preds","ensemble.preds")

for (i in seq_along(models)) {
  
  performance <- get_performance(model.preds = models[i])
  
  fwrite(data.frame(pred = performance) , file = paste0("modeling/performance_measures/", "perf_model_", model_name[[i]], ".csv"))
  
}



