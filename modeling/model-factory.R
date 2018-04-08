library(randomForest)
library(rpart)
library(e1071)

classification_svm <- function(control, ...){
  classification <- svm(type="C-classification", kernel="sigmoid", epsilon=control$epsilon, cost=control$cost, ...=...)
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
  
  if(model_name=="svm"){
    model_function = classification_svm
    prediction_type = "decision"
  }
  
  return(
    list(model_function= model_function, 
         prediction_type = prediction_type))
}



