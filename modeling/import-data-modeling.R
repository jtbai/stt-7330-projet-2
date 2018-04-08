# -------------------------------------------------------------------------
# Title: import-data-modeling.R
# Goal: Import data for modeling and fix formats
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(data.table)


# Import data -------------------------------------------------------------

data_modeling_classical_methods <- fread("data/data_modeling_classical_methods.csv")
data_modeling_classical_methods[, surface := as.factor(surface)]


# Split train and test ----------------------------------------------------

data_modeling_classical_methods_train <- data_modeling_classical_methods[split_group == 1, -(c("split_group")), with = FALSE]
data_modeling_classical_methods_validation <- data_modeling_classical_methods[split_group == 2, -(c("split_group")), with = FALSE]


