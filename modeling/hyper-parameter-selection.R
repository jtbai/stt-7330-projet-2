library(doParallel)
library(foreach)


add_kfold_information_to_dataset <- function(dataset, number_of_k_fold){
  set.seed(1337)
  number_of_data_point = nrow(dataset)
  number_of_data_per_fold = floor(number_of_data_point/number_of_k_fold)
  number_of_leftover_points = number_of_data_point %% number_of_k_fold
  unrandomised_kfold_class = c(rep(1:number_of_k_fold, number_of_data_per_fold), sample(1:number_of_k_fold,size = number_of_leftover_points, replace = FALSE))
  kfold = sample(unrandomised_kfold_class, number_of_data_point, replace=FALSE)
  
  return(cbind(dataset, kfold))
}

obtain_sub_train_test <- function(kfoldable_dataset, fold){
  sub_train <- kfoldable_dataset %>% filter(kfold != fold)
  sub_test <- kfoldable_dataset  %>% filter(kfold == fold)
  
  return(list(train=sub_train, test=sub_test))
}

get_calculations_to_do <- function(number_of_hyper_parameter_to_test, number_of_kfolds){
  index_to_run = matrix(nrow=number_of_hyper_parameter_to_test*number_of_kfolds, ncol=3)
  colnames(index_to_run) = c("hyper_parameter_id", "kfold", "result")
  job_id = 1  
  for(hyper_parameter_index in 1:number_of_hyper_parameter_to_test){
    for(current_kfold in 1:number_of_kfolds){
      index_to_run[job_id, 1] = hyper_parameter_index
      index_to_run[job_id, 2] = current_kfold
      index_to_run[job_id, 3] = NA
      job_id = job_id+1
    }
  } 
  
  return(index_to_run)
}

get_calculation_inputs <- function(hyper_parameter_grid, hyper_parameter_to_use, kfoldable_data, kfold_to_use){
  sub_data <-  obtain_sub_train_test(kfoldable_data, kfold_to_use)
  sub_data_train <- sub_data$train
  sub_data_test = sub_data$test
  current_hyper_parameters  = hyper_parameter_grid[hyper_parameter_to_use, ]
  calculation_input = list(train = sub_data_train, test = sub_data_test, hyper_parameters=current_hyper_parameters)
  
  return(calculation_input)
}

calculate_fold_result <- function(model_function, train_data, test_data, hyper_parameter){
  model <- model_function(surface ~ ., data = train_data,   control = hyper_parameter)
  out = predict(model, test_data, type = "class") 
  current_result <- sum(out == test_data$surface) / nrow(test_data)
  
  return(current_result)
}

calculate_job <- function(hyper_parameter_and_fold_index_as_vector, model_function, kfoldable_dataset, hyper_parameter_grid){
  hyper_parameter_index = hyper_parameter_and_fold_index_as_vector[1]
  kfold_index = hyper_parameter_and_fold_index_as_vector[2]
  calculation_input = get_calculation_inputs(hyper_parameter_grid, hyper_parameter_index, kfoldable_dataset, kfold_index)
  current_result = calculate_fold_result(model_function,calculation_input$train, calculation_input$test, calculation_input$hyper_parameters)

  return(list(kfold=kfold_index, hyper_parameter =calculation_input$hyper_parameters, result = current_result))
}

get_best_hyper_parameter_index <- function(result_as_matrix){
  results = as.data.frame(result_as_matrix)
  best_hyper_parameter <- results %>% 
    group_by(hyper_parameter_id) %>%
    summarise(result_by_hyper_parameter = mean(result)) %>%
    filter(result_by_hyper_parameter == max(result_by_hyper_parameter)) %>%
    filter(hyper_parameter_id == min(hyper_parameter_id))
  
  return(best_hyper_parameter$hyper_parameter_id)
}

get_best_hyper_parameters <- function(dataset, model_name, number_of_kfolds, parameter_list_to_grid){
  
  kfoldable_train_data <- add_kfold_information_to_dataset(dataset, number_of_kfolds)
  
  hyper_parameter_grid <- cross_df(parameter_list_to_grid)
  number_of_hyper_parameter_sets = nrow(hyper_parameter_grid)

  model_function = get_model_function(model_name) 
  calculations_to_do = get_calculations_to_do(number_of_hyper_parameter_sets, number_of_kfolds)
  
  
  
  for(calculation in 1:nrow(calculations_to_do)) {
    indices_to_use = calculations_to_do[calculation, ]
    job_result = calculate_job(indices_to_use, model_function, kfoldable_train_data, hyper_parameter_grid)
    calculations_to_do[calculation, 3] = job_result$result
    print(paste0("hyper-parameters: ",paste(job_result$hyper_parameter, collapse = "-")," kfold: ",job_result$kfold," - Result :", job_result$result))
  }
  
  
  best_hyper_parameter_index = get_best_hyper_parameter_index(calculations_to_do)
  
  return(hyper_parameter_grid[best_hyper_parameter_index, ])
}
