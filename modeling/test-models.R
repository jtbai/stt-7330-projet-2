# -------------------------------------------------------------------------
# Title: test-models
# Goal: Test few models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(e1071)
library(randomForest)
library(rpart)
library(MASS)
library(nnet)
library(tidyverse)
library(caret)
library(randomForest)

# Import data -------------------------------------------------------------

source("data/scripts/clean-data.R")
source("data/scripts/preprocess-data.R")

data_prepared <-  data_imputed
# Model -------------------------------------------------------------------

# Split train/test
test_index <- sample(1:nrow(data_prepared), 0.2 * nrow(data_prepared))
data_train <- data_prepared[-test_index,]
data_test <- data_prepared[test_index,]

#K-fold setup
add_kfold_information_to_dataset <- function(dataset, number_of_k_fold){
  number_of_data_point = nrow(dataset)
  number_of_data_per_fold = floor(number_of_data_point/number_of_k_folds)
  number_of_leftover_points = number_of_data_point %% number_of_k_folds
  unrandomised_kfold_class = c(rep(1:number_of_k_folds, number_of_data_per_fold), sample(1:number_of_k_folds,size = number_of_leftover_points, replace = FALSE))
  kfold = sample(unrandomised_kfold_class, number_of_data_point, replace=FALSE)
  
  return(cbind(dataset, kfold))
}

obtain_sub_train_test <- function(kfoldable_dataset, fold){
  sub_train <- kfoldable_dataset %>% filter(kfold != fold)
  sub_test <- kfoldable_dataset  %>% filter(kfold == fold)
  
  return(list(train=sub_train, test=sub_test))
}

kfoldable_train_data <- add_kfold_information_to_dataset(data_train, 10)


# Naive bayes
model_bayes <-  naiveBayes(surface ~ ., data = data_train)
out <- predict(model_bayes, data_test)
sum(out == data_test[, surface]) / nrow(data_test)

# Tree-based
hyper_parameter_grid <- cross_df(list(minsplit = c(2, 5, 10, 20, 50),
                      maxdepth = c(1, 3, 8, 15, 20)))
number_of_hyper_parameter_sets = nrow(hyper_parameter_grid)
results_by_hyper_parameters <- numeric(number_of_hyper_parameter_sets)
for(hyper_parameter_index in 1:number_of_hyper_parameter_sets){
  current_hyper_parameters  = hyper_parameter_grid[hyper_parameter_index, ]
  fold_results <- numeric(number_of_k_folds)
  for(current_fold in 1:number_of_k_folds){
    sub_data <-  obtain_sub_train_test(kfoldable_train_data, current_fold)
    sub_data_train <- sub_data$train
    sub_data_test = sub_data$test
    model_tree <- rpart(surface ~ ., data = sub_data_train,   control = current_hyper_parameters)
    out = predict(model_tree, sub_data_test, type = "class") 
    current_result <- sum(out == sub_data_test$surface) / nrow(sub_data_test)
    fold_results[current_fold] <- current_result
    print(paste0("Params ",paste(current_hyper_parameters, collapse = "-")," - ","fold ",current_fold, ": accuracy = ", current_result))
  }
  results_by_hyper_parameters[hyper_parameter_index] = mean(fold_results)
}

best_hyper_parameters = hyper_parameter_grid[min(which(results_by_hyper_parameters == max(results_by_hyper_parameters))), ]

model_tree <- rpart(surface ~ ., data = data_train, control = best_hyper_parameters)
out <- predict(model_tree, data_test, type = "class")
sum(out == data_test[, surface]) / nrow(data_test)

plot(model_tree, margin = 0.1)
text(model_tree, use.n = T, cex = 0.8)

# LDA
model_lda <- lda(surface ~ ., data = data_train)
out <- predict(model_lda, data_test)$class
sum(out == data_test[, surface]) / nrow(data_test)

# QDA
model_qda <- qda(surface ~ ., data = data_train)
out <- predict(model_qda, data_test)$class
sum(out == data_test[, surface]) / nrow(data_test)

# Random Forrest
hyper_parameter_grid <- cross_df(
  list(mtry = seq(4,16,4), ntree = c(700, 1000,2000))
  )

number_of_hyper_parameter_sets = nrow(hyper_parameter_grid)
results_by_hyper_parameters <- numeric(number_of_hyper_parameter_sets)
for(hyper_parameter_index in 1:number_of_hyper_parameter_sets){
  current_hyper_parameters  = hyper_parameter_grid[hyper_parameter_index, ]
  fold_results <- numeric(number_of_k_folds)
  for(current_fold in 1:number_of_k_folds){
    sub_data <-  obtain_sub_train_test(kfoldable_train_data, current_fold)
    sub_data_train <- sub_data$train
    sub_data_test = sub_data$test
    
    model = randomForest(surface ~ ., data =sub_data_train,   control = current_hyper_parameters)
    out = predict(model, sub_data_test, type = "class") 
    
    current_result <- sum(out == sub_data_test$surface) / nrow(sub_data_test)
    fold_results[current_fold] <- current_result
    print(paste0("Params ",paste(current_hyper_parameters, collapse = "-")," - ","fold ",current_fold, ": accuracy = ", current_result))
  }
  results_by_hyper_parameters[hyper_parameter_index] = mean(fold_results)
}


best_hyper_parameters = hyper_parameter_grid[min(which(results_by_hyper_parameters == max(results_by_hyper_parameters))), ]


model_rf <- randomForest(surface ~ ., data =sub_data_train,   control = best_hyper_parameters)
out <- predict(model_rf, data_test)
sum(out == data_test[, surface]) / nrow(data_test)
varImpPlot(model_rf)

# Model logistic
model_multi <- multinom(surface ~ ., data = data_train)
out <- predict(model_multi, data_test)
sum(out == data_test[, surface]) / nrow(data_test)

# SVM



# Hasard
props <- tabulate(as.numeric(data_test$surface), nbins = 3) / nrow(data_test)
1 - sum(props^2)
