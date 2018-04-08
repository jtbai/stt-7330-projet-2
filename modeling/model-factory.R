library(randomForest)
library(rpart)

get_model_function <- function(model_name){
  model_function = NULL
  if(model_name=="random_forest"){
    model_function = randomForest
  }
  
  if(model_name=="tree_based"){
    model_function = rpart
  }
  
  return(model_function)
}



