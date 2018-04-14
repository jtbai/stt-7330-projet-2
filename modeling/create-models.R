# -------------------------------------------------------------------------
# Title: create-models
# Goal: This script aims to create/train/predict our different models
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

# Source functions --------------------------------------------------------

source("modeling/model-factory.R")
source("modeling/hyper-parameter-selection.R")


# Define global configuration ---------------------------------------------

# Do grid search or not
ind_do_grid_search <- TRUE

# 
train_groups <- 1L
test_groups <- 2L


# Create models -----------------------------------------------------------



if (ind_do_grid_search) {
  
}


