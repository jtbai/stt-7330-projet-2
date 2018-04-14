# -------------------------------------------------------------------------
# Title: grid-search
# Goal: Find optimal parameters for each models through grid search
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(e1071)
library(MASS)
library(nnet)
library(data.table)
library(rpart)
library(jsonlite)


source("modeling/model-factory.R")
source("modeling/hyper-parameter-selection.R")

# Import data -------------------------------------------------------------

data_prepared <- fread("data/data_modeling_classical_methods.csv")
data_prepared[, surface := as.factor(surface)]


# Split train/test --------------------------------------------------------

train_groups <- 1L
test_groups <- 2L

data_train <- data_prepared[split_group %in% train_groups, -(c("split_group")), with = FALSE]
data_test <- data_prepared[split_group %in% test_groups, -(c("split_group")), with = FALSE]


# Search optimal models ---------------------------------------------------

# Tree-based
hyper_parameter <- list(minsplit = c(2, 5, 10), maxdepth = c(1, 3, 5, 8))
best_hyper_parameters_tree <-  get_best_hyper_parameters(data_train, "tree_based", 10, hyper_parameter)

# Random Forrest
hyper_parameter <- list(mtry = seq(4, 16, 4), ntree = c(700, 1000, 2000))
best_hyper_parameters_rf <-  get_best_hyper_parameters(data_train, "random_forest", 10, hyper_parameter)

# SVM - gaussien
hyper_parameter <- list(epsilon = c(0.1, 0.01, 0.001), cost = c(0.90, 0.95, 0.99, 1))
best_hyper_parameters_svm_gaussien <-  get_best_hyper_parameters(data_train, "svm_gaussien", 10, hyper_parameter)

# SVM - polynomial
hyper_parameter <- list(epsilon = c(0.1, 0.01, 0.001), cost = c(0.90, 0.95, 0.99, 1))
best_hyper_parameters_svm_poly3 <-  get_best_hyper_parameters(data_train, "svm_poly3", 10, hyper_parameter)


# Export parameters -------------------------------------------------------

write_df_to_json <- function(data, path) {
  
  data %>% 
    toJSON(dataframe = "rows") %>% 
    write_lines(path)
  
}

vecteurs_hyper_param <- list(best_hyper_parameters_tree, best_hyper_parameters_rf, best_hyper_parameters_svm_gaussien, best_hyper_parameters_svm_poly3)
lapply(vecteurs_hyper_param, function(x){
  write_df_to_json(x, "modeling/parameters-selection/")
})

