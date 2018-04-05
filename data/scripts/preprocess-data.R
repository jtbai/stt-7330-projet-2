# -------------------------------------------------------------------------
# Title: preprocess-data
# Goal: Prepare data for modeling (FE and Imputations)
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(mltools)
library(data.table)
library(tidyr)


# Missing data imputation -------------------------------------------------

# Because we do not have a lot of missing data I'll just exclude them for the moment
# Imputation method: Complete case analyses

data_imputed <- data_clean %>% drop_na()

# Features engineering ----------------------------------------------------

# One-hot categorical variables

# vec_categorical_variables <- c("winner_hand", "loser_hand")
# data_imputed[, (vec_categorical_variables) := lapply(.SD, as.factor), .SDcol = vec_categorical_variables]
# data_imputed <- one_hot(data_imputed, cols = vec_categorical_variables)

# Change the response variable to a factor
data_imputed[, surface := as.factor(surface)]


