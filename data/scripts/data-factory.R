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

set.seed(666) # Number of the beast


# Source the functions needed for the pipeline ----------------------------

source("data/scripts/import-data.R")
source("data/scripts/clean-data.R")
source("data/scripts/feature-engineering-data-classical-methods.R")
source("data/scripts/features-engineering-data-neural-net.R")
source("data/scripts/imputation-data.R")
source("data/scripts/split-data-modeling.R")


# Create datasets ---------------------------------------------------------

# Import and clean
data_raw <- import_games(year_from = year_from, year_to = year_to)
data_clean <- clean_data_from_raw(dt = data_raw)

# Create features
data_classical_methods_with_features <- create_features_of_classical_modeling(dt = data_clean)
data_neural_net_with_features <- create_features_of_neural_net(dt = data_clean)

# Split into train and test
data_classical_methods_splitted <- split_data_for_modeling(data_classical_methods_with_features)
data_neural_net_splitted <- split_data_for_modeling(data_neural_net_with_features)

# Missing data imputation
data_modeling_classical_methods <- do_missing_data_imputation(dt = data_classical_methods_splitted)
data_modeling_neural_net <- do_missing_data_imputation(dt = data_neural_net_splitted)

# Drop some variables
var_to_keep <- c("surface", "ind_retired", "nb_tie_break", "ind_max_sets", "ind_min_sets", "winner_ace_svpt", "winner_1stwon_1stin", "winner_1stin_svpt", "winner_df_svpt", "winner_min_svpt", "winner_1stwon_servewon", "winner_serve_won", "winner_break_pts", "loser_ace_svpt", "loser_1stwon_1stin", "loser_1stin_svpt", "loser_df_svpt", "loser_min_svpt", "loser_1stwon_servewon", "loser_serve_won", "loser_break_pts", paste0("difference_score_set_", seq(1, 5)), "split_group")
data_modeling_classical_methods <- data_modeling_classical_methods[, (var_to_keep), with = FALSE]


# Save data for modeling --------------------------------------------------

fwrite(data_modeling_classical_methods, file = "data/data_modeling_classical_methods.csv")
fwrite(data_modeling_neural_net, file = "data/data_modeling_neural_net.csv")
