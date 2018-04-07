# -------------------------------------------------------------------------
# Title: clean-data
# Goal: This script is aimed to functionnalize the cleaning operations on raw dataset
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

clean_data_from_raw <- function(dt){
  
  # Remove matches played at Olympics, Davis Cup and matches played on Carpet surface (not enough game)
  data_clean <- copy(dt)[-grep(pattern = "Davis", x = dt$tourney_name),]
  data_clean[-grep(pattern = "Olympics", x = data_clean$tourney_name),]
  data_clean <- data_clean[surface != "Carpet",]
  
  # Remove all variables directly linked to the match surface
  col_linked_to_match <- c("tourney_id", "tourney_name", "tourney_date", "draw_size", "tourney_level", "round", "match_num", "winner_seed", "loser_seed")
  data_clean[, (col_linked_to_match) := NULL]
  
  # Remove unrelevant variables
  col_unrelevant <- c("winner_id", "winner_name", "loser_id", "loser_name", "winner_entry", "loser_entry", "winner_rank", "winner_rank_points", "loser_rank_points", "loser_rank", "winner_ht", "loser_ht")
  data_clean[, (col_unrelevant) := NULL]

  # Features engineering ----------------------------------------------------
  
  # We create ind for retired
  data_clean[, ind_retired := FALSE]
  data_clean[grep("RET", score), ind_retired := TRUE]
  
  # We drop walk over
  data_clean <- data_clean[!grep("W/O", score),]
  
  # We have to do something with the "score" variable
  data_clean[, winner_score := str_extract_all(score, "\\d+(?=\\-)")]
  data_clean[, loser_score := str_extract_all(score, "(?<=-)\\d+")]
  
  # I do replace missing set by 0 instead of NA (ce sont les sets qui n'ont pas été joués)
  for (set in 1:5){
    data_clean[, paste0("winner_score_", set) := unlist(lapply(.I, function(i){
      ifelse(is.na(unlist(winner_score[i])[set]), NA, as.numeric(unlist(winner_score[i])[set]))
    }))]
    data_clean[, paste0("loser_score_", set) := unlist(lapply(.I, function(i){
      ifelse(is.na(unlist(loser_score[i])[set]), NA, as.numeric(unlist(loser_score[i])[set]))
    }))]
  }
  data_clean[, (c("winner_score", "loser_score")) := NULL]
  
  # Change the response variable to a factor
  data_clean[, surface := ifelse(surface == "Hard", 1, ifelse(surface == "Clay", 2, 3))]
  
  return(data_clean)
}


