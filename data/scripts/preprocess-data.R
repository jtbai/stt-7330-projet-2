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
library(stringr)


# Features engineering ----------------------------------------------------

# Number of tie break
data_clean[, nb_tie_break := str_count(score, "7")]

# Indicator game played to the limit of sets
data_clean[, ind_max_sets := ifelse((5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == best_of, TRUE, FALSE)]

# Indicator game played to the min of stes
data_clean[, ind_min_sets := FALSE]
data_clean[best_of == 3 & (5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == 2, ind_min_sets := TRUE]
data_clean[best_of == 5 & (5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == 3, ind_min_sets := TRUE]

# % of aces / number of points at serve
data_clean[, winner_ace_svpt := w_ace/w_svpt]
data_clean[, loser_ace_svpt := l_ace/l_svpt]

# % 1st serve won / first serve in
data_clean[, winner_1stwon_1stin := w_1stWon/w_1stIn]
data_clean[, loser_1stwon_1stin := l_1stWon/l_1stIn]

# % 1st serve in / svpt
data_clean[, winner_1stin_svpt := w_1stIn/w_svpt]
data_clean[, loser_1stin_svpt := l_1stIn/l_svpt]

# % df / svpt
data_clean[, winner_df_svpt := w_df/w_svpt]
data_clean[, loser_df_svpt := l_df/l_svpt]

# Minutes played / svpt
data_clean[, winner_min_svpt := minutes/w_svpt]
data_clean[, loser_min_svpt := minutes/l_svpt]

# % first serve won / server won
data_clean[, winner_1stwon_servewon := w_1stWon/(w_1stWon + w_2ndWon)]
data_clean[, loser_1stwon_servewon := l_1stWon/(l_1stWon + l_2ndWon)]

# % serve won / server won
data_clean[, winner_serve_won := (w_1stWon + w_2ndWon)/w_svpt]
data_clean[, loser_serve_won := (l_1stWon + l_2ndWon)/l_svpt]

# Number of breaks
data_clean[, winner_break_pts := (l_bpFaced - l_bpSaved)]
data_clean[, loser_break_pts := (w_bpFaced - w_bpSaved)]


# Missing data imputation -------------------------------------------------

# We replace ratios that have denominator of 0 by 0 instead of NaN
data_clean[w_1stIn == 0, winner_1stwon_1stin := 0]
data_clean[l_1stIn == 0, loser_1stwon_1stin := 0]
data_clean[(w_1stWon + w_2ndWon) == 0, winner_1stwon_servewon := 0]
data_clean[(l_1stWon + l_2ndWon) == 0, loser_1stwon_servewon := 0]


# Keep some features
var_to_keep <- c("surface", "ind_retired", "nb_tie_break", "ind_max_sets", "ind_min_sets", "winner_ace_svpt", "winner_1stwon_1stin", "winner_1stin_svpt", "winner_df_svpt", "winner_min_svpt", "winner_1stwon_servewon", "winner_serve_won", "winner_break_pts", "loser_ace_svpt", "loser_1stwon_1stin", "loser_1stin_svpt", "loser_df_svpt", "loser_min_svpt", "loser_1stwon_servewon", "loser_serve_won", "loser_break_pts")
data_clean <- data_clean[, (var_to_keep), with = FALSE]

# Because we do not have a lot of missing data I'll just exclude them for the moment
# Imputation method: Complete case analyses

data_imputed <- data_clean %>% drop_na()


# Prepare for modeling ----------------------------------------------------

# Change the response variable to a factor
data_imputed[, surface := as.factor(surface)]


