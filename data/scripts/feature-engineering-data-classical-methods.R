# -------------------------------------------------------------------------
# Title: preprocess-data
# Goal: This script is aimed to functionalize the features engineering of the classic models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

# Features engineering ----------------------------------------------------

create_features_of_classical_modeling <- function(dt, split_variables){
  
  if (split_variables){
    
    # Number of tie break
    data_featured <- copy(dt)[, nb_tie_break := str_count(score, "7")]
    
    # Indicator game played to the limit of sets
    data_featured[, ind_max_sets := ifelse((5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == best_of, TRUE, FALSE)]
    
    # Indicator game played to the min of stes
    data_featured[, ind_min_sets := FALSE]
    data_featured[best_of == 3 & (5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == 2, ind_min_sets := TRUE]
    data_featured[best_of == 5 & (5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == 3, ind_min_sets := TRUE]
    
    # % of aces / number of points at serve
    data_featured[, winner_ace_svpt := w_ace/w_svpt]
    data_featured[, loser_ace_svpt := l_ace/l_svpt]
    
    # % 1st serve won / first serve in
    data_featured[, winner_1stwon_1stin := w_1stWon/w_1stIn]
    data_featured[, loser_1stwon_1stin := l_1stWon/l_1stIn]
    
    # % 1st serve in / svpt
    data_featured[, winner_1stin_svpt := w_1stIn/w_svpt]
    data_featured[, loser_1stin_svpt := l_1stIn/l_svpt]
    
    # % df / svpt
    data_featured[, winner_df_svpt := w_df/w_svpt]
    data_featured[, loser_df_svpt := l_df/l_svpt]
    
    # Minutes played / svpt
    data_featured[, winner_min_svpt := minutes/w_svpt]
    data_featured[, loser_min_svpt := minutes/l_svpt]
    
    # % first serve won / server won
    data_featured[, winner_1stwon_servewon := w_1stWon/(w_1stWon + w_2ndWon)]
    data_featured[, loser_1stwon_servewon := l_1stWon/(l_1stWon + l_2ndWon)]
    
    # % serve won / server won
    data_featured[, winner_serve_won := (w_1stWon + w_2ndWon)/w_svpt]
    data_featured[, loser_serve_won := (l_1stWon + l_2ndWon)/l_svpt]
    
    # Number of breaks
    data_featured[, winner_break_pts := (l_bpFaced - l_bpSaved)]
    data_featured[, loser_break_pts := (w_bpFaced - w_bpSaved)]
    
    # Missing data imputation -------------------------------------------------
    
    # We replace ratios that have denominator of 0 by 0 instead of NaN
    data_featured[w_1stIn == 0, winner_1stwon_1stin := 0]
    data_featured[l_1stIn == 0, loser_1stwon_1stin := 0]
    data_featured[(w_1stWon + w_2ndWon) == 0, winner_1stwon_servewon := 0]
    data_featured[(l_1stWon + l_2ndWon) == 0, loser_1stwon_servewon := 0]
    data_featured[w_svpt == 0, (c("winner_ace_svpt", "winner_1stin_svpt", "winner_df_svpt", "winner_min_svpt", "winner_serve_won")) := 0]
    data_featured[l_svpt == 0, (c("loser_ace_svpt", "loser_1stin_svpt", "loser_df_svpt", "loser_min_svpt", "loser_serve_won")) := 0]
    
  } else {
    
    # Number of tie break
    data_featured <- copy(dt)[, nb_tie_break := str_count(score, "7")]
    
    # Indicator game played to the limit of sets
    data_featured[, ind_max_sets := ifelse((5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == best_of, TRUE, FALSE)]
    
    # Indicator game played to the min of stes
    data_featured[, ind_min_sets := FALSE]
    data_featured[best_of == 3 & (5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == 2, ind_min_sets := TRUE]
    data_featured[best_of == 5 & (5 - (is.na(winner_score_1) + is.na(winner_score_2) + is.na(winner_score_3) + is.na(winner_score_4) + is.na(winner_score_5))) == 3, ind_min_sets := TRUE]
    
    # % of aces / number of points at serve
    data_featured[, ace_by_svpt := (w_ace + l_ace)/(w_svpt + l_svpt)]
    
    # % 1st serve won / first serve in
    data_featured[, first_won_by_first_in := (w_1stWon + l_1stWon)/(w_1stIn + l_1stIn)]
    
    # % 1st serve in / svpt
    data_featured[, first_in_by_svpt := (w_1stIn + l_1stIn)/(w_svpt + l_svpt)]
    
    # % df / svpt
    data_featured[, df_by_svpt := (w_df + l_df)/(w_svpt + l_svpt)]
    
    # Minutes played / svpt
    data_featured[, min_by_svpt := minutes/(w_svpt + l_svpt)]
    
    # % first serve won / server won
    data_featured[, first_won_by_serve_won := (w_1stWon + l_1stWon)/(w_1stWon + w_2ndWon + l_1stWon + l_2ndWon)]
    
    # % serve won / server won
    data_featured[, serve_won_by_serve_pts := (w_1stWon + w_2ndWon + l_1stWon + l_2ndWon)/(w_svpt + l_svpt)]
    
    # Number of breaks
    data_featured[, nb_break_pts := (w_bpFaced - w_bpSaved) + (l_bpFaced - l_bpSaved)]
    
    
    # Missing data imputation -------------------------------------------------
    
    # We replace ratios that have denominator of 0 by 0 instead of NaN
    data_featured[(w_1stIn + l_1stIn) == 0, first_won_by_first_in := 0]
    data_featured[(w_1stWon + w_2ndWon + l_1stWon + l_2ndWon) == 0, first_won_by_serve_won := 0]
    data_featured[(w_svpt + l_svpt) == 0, (c("ace_by_svpt", "first_in_by_svpt", "df_by_svpt", "min_by_svpt", "serve_won_by_serve_pts")) := 0]
  }
  
  # Create diff between scores of each sets
  # data_featured[, difference_score_set_1 := ifelse(is.na(winner_score_1) | is.na(loser_score_1), 0, winner_score_1 - loser_score_1)]
  # data_featured[, difference_score_set_2 := ifelse(is.na(winner_score_2) | is.na(loser_score_2), 0, winner_score_2 - loser_score_2)]
  # data_featured[, difference_score_set_3 := ifelse(is.na(winner_score_3) | is.na(loser_score_3), 0, winner_score_3 - loser_score_3)]
  # data_featured[, difference_score_set_4 := ifelse(is.na(winner_score_4) | is.na(loser_score_4), 0, winner_score_4 - loser_score_4)]
  # data_featured[, difference_score_set_5 := ifelse(is.na(winner_score_5) | is.na(loser_score_5), 0, winner_score_5 - loser_score_5)]
  
  variables_scores <- c(paste0("winner_score_", seq(1, 5)), paste0("loser_score_", seq(1, 5)))
  data_featured[, (variables_scores) := NULL]
  
  return(data_featured)
  
}

