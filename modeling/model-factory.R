# -------------------------------------------------------------------------
# Title: model-factory
# Goal: Create interface fucntions for each model
# Date: April 2018
# Author: JT Baillargeon
# -------------------------------------------------------------------------

classification_svm_gaussien <- function(control, ...){
  classification <- svm(type="C-classification", kernel="sigmoid", epsilon=control$epsilon, cost=control$cost, ...=...)
}

classification_svm_poly3 <- function(control, ...){
  classification <- svm(type="C-classification", kernel="polynomial", epsilon=control$epsilon, cost=control$cost, ...=...)
}

get_model_function <- function(model_name){
  model_function = NULL
  prediction_type = NULL
  
  if(model_name=="random_forest"){
    model_function = randomForest
    prediction_type = "class"
  }
  
  if(model_name=="tree_based"){
    model_function = rpart
    prediction_type = "class"
  }
  
  if(model_name=="svm_gaussien"){
    model_function = classification_svm_gaussien
    prediction_type = "decision"
  }

  if(model_name=="svm_poly3"){
    model_function = classification_svm_poly3
    prediction_type = "decision"
  }
  
  if(model_name=="bayes"){
    model_function = naiveBayes
    prediction_type = "raw"
  }
  
  if(model_name=="lda"){
    model_function = lda
    prediction_type = "class"
  }
  
  if(model_name=="qda"){
    model_function = qda
    prediction_type = "class"
  }
  
  if(model_name=="multinomial"){
    model_function = multinom
    prediction_type = NA
  }
  
  return(
    list(model_function= model_function, 
         prediction_type = prediction_type))
}



