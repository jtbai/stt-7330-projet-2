# -------------------------------------------------------------------------
# Title: train-models
# Goal: Train models by finding optimal parameters through grid search and save models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


do_grid_search <- function(data_train, data_test, path_models){
  
# Train models ------------------------------------------------------------
  
  # Naive bayes
  model_bayes <-  naiveBayes(surface ~ ., data = data_train)
  
  # LDA
  model_lda <- lda(surface ~ ., data = data_train)
  
  # QDA
  model_qda <- qda(surface ~ ., data = data_train)
  
  # Tree-based
  hyper_parameter <- list(minsplit = c(2, 5, 10), maxdepth = c(1, 3, 5, 8))
  best_hyper_parameters_tree <-  get_best_hyper_parameters(data_train, "tree_based", 10, hyper_parameter)
  model_function = get_model_function("tree_based")$model_function
  model_tree <- model_function(surface ~ ., data = data_train, control = best_hyper_parameters_tree)
  
  # Random Forrest
  hyper_parameter <- list(mtry = seq(4, 16, 4), ntree = c(700, 1000, 2000))
  best_hyper_parameters_rf <-  get_best_hyper_parameters(data_train, "random_forest", 10, hyper_parameter)
  model_function = get_model_function("random_forest")$model_function
  model_rf <- model_function(surface ~ ., data =data_train,   control = best_hyper_parameters_rf)
  
  # SVM - gaussien
  hyper_parameter <- list(epsilon = c(0.1, 0.01, 0.001), cost = c(0.90, 0.95, 0.99, 1))
  best_hyper_parameters_svm_gaussien <-  get_best_hyper_parameters(data_train, "svm_gaussien", 10, hyper_parameter)
  model_function = get_model_function("svm_gaussien")$model_function
  model_svm_gaussien <- model_function(surface ~ ., data =data_train, control= best_hyper_parameters_svm_gaussien)
  
  # SVM - polynomial
  hyper_parameter <- list(epsilon = c(0.1, 0.01, 0.001), cost = c(0.90, 0.95, 0.99, 1))
  best_hyper_parameters_svm_poly3 <-  get_best_hyper_parameters(data_train, "svm_poly3", 10, hyper_parameter)
  model_function = get_model_function("svm_poly3")$model_function
  model_svm_poly3 <- model_function(surface ~ ., data =data_train, control= best_hyper_parameters_svm_poly3)
  
  # Modele multinomiale
  model_multi <- multinom(surface ~ ., data = data_train)
  
  list_trained_models <- list(model_bayes, model_lda, model_qda, model_tree, model_rf, model_svm_gaussien, model_svm_poly3, model_multi)
  names(list_trained_models) <- c("model_bayes", "model_lda", "model_qda", "model_tree", "model_rf", "model_svm_gaussien", "model_svm_poly3", "model_multi")

# Export models -----------------------------------------------------------

  for (model in seq_along(list_trained_models)){
    saveRDS(object = list_trained_models[model], file = paste0(path, names(list_trained_models[model]), ".rds"))
  }
  
}
