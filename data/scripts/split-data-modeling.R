# -------------------------------------------------------------------------
# Title: split-data-modeling
# Goal: This script is aimed to split the data into train and test (also split the train)
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

split_data_for_modeling <- function(dt, split_percentage = 0.9){
  
  set.seed(666) # Number of the beast
  
  # Shuffle the data
  data_shuffled <- copy(dt)[sample(nrow(dt)),]
  
  # Groupe 1: train (train models)
  # Groupe 2: validation (evaluate performance)
  # Groupe 3: test (test the chosen model at the end)
  
  # Create a new split variable
  index_train <- sample(1:nrow(data_shuffled), split_percentage * nrow(data_shuffled))
  index_group_2 <- sample(index_train, (1 - split_percentage) * length(index_train))
  index_group_1 <- index_train[-which(index_train %in% index_group_2)] 
  index_group_3 <- seq(1:nrow(data_shuffled))[-index_train]
  
  data_shuffled[index_group_1, split_group := 1]
  data_shuffled[index_group_2, split_group := 2]
  data_shuffled[index_group_3, split_group := 3]
  
}