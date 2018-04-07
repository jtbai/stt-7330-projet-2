# -------------------------------------------------------------------------
# Title: data-factory
# Goal: This script is aimed to create the datasets needed for modeling
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(data.table)
library(RCurl)
library(stringr)
library(mltools)
library(tidyr)


# Define import parameters ------------------------------------------------

# Define from/to which year you want to import data
year_from <- 2005
year_to <- 2017


# Source the functions needed for the pipeline ----------------------------

source("data/scripts/import-data.R")
source("data/scripts/clean-data.R")
source("data/scripts/feature-engineering-data-classical-methods.R")
source("data/scripts/imputation-data.R")


# Create datasets ---------------------------------------------------------

data_raw <- import_games(year_from = year_from, year_to = year_to)
data_clean <- clean_data_from_raw(dt = data_raw)

# Data for classical methods
data_with_features <- create_features_of_classical_modeling(dt = data_clean)
var_to_keep <- c("surface", "ind_retired", "nb_tie_break", "ind_max_sets", "ind_min_sets", "winner_ace_svpt", "winner_1stwon_1stin", "winner_1stin_svpt", "winner_df_svpt", "winner_min_svpt", "winner_1stwon_servewon", "winner_serve_won", "winner_break_pts", "loser_ace_svpt", "loser_1stwon_1stin", "loser_1stin_svpt", "loser_df_svpt", "loser_min_svpt", "loser_1stwon_servewon", "loser_serve_won", "loser_break_pts", paste0("difference_score_set_", seq(1, 5)))
data_with_features <- data_with_features[, (var_to_keep), with = FALSE]

# Data for neural networks


data_modeling_classical_methods <- do_missing_data_imputation(dt = data_with_features)
data_modeling_neural_net <- do_missing_data_imputation(dt = data_clean)
