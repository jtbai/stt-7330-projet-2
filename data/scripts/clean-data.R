# -------------------------------------------------------------------------
# Title: clean-data
# Goal: Clean raw data and do some features engineering for modeling
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(data.table)
library(stringr)


# Clean data --------------------------------------------------------------

data_raw <- fread("data/data-raw/data-raw.csv")

# Remove matches played at Olympics, Davis Cup and matches played on Carpet surface (not enough game)
data_clean <- copy(data_raw)[-grep(pattern = "Davis", x = data_raw$tourney_name),]
data_clean[-grep(pattern = "Olympics", x = data_clean$tourney_name),]
data_clean <- data_clean[surface != "Carpet",]

# Remove all variables directly linked to the match surface
col_linked_to_match <- c("tourney_id", "tourney_name", "tourney_date", "draw_size", "tourney_level")
data_clean[, (col_linked_to_match) := NULL]

# Remove unrelevant variables
col_unrelevant <- c("winner_id", "winner_name", "loser_id", "loser_name", "winner_entry", "loser_entry", "winner_ioc", "loser_ioc")
data_clean[, (col_unrelevant) := NULL]


# Explore data ------------------------------------------------------------

unique(data_clean$surface)
# 3 types of surface : OK !!!

table(data_clean$surface)
# Less matches on grass ... normal !

paste0(round(100*apply(apply(data_clean, 2, is.na), 2, sum)/nrow(data_clean)), " % NA")
colnames(data_clean)[c(3, 11, 14)]
# Seems normal to me concerning the seed (players that do not have seed probably just do not have at the beggining of a tournament)
# Not that much missing values in stats (1%) .. maybe we could just drop them (Méthode imputation: Anlyse des cas complets)

# There is something to do with the missing seeds but for the moment I'll exclude this variable
data_clean[, (c("winner_seed", "loser_seed")) := NULL]
paste0(round(100*apply(apply(data_clean, 2, is.na), 2, sum)/nrow(data_clean)), " % NA")
colnames(data_clean)[c(5, 12)]

data_clean[, (c("winner_ht", "loser_ht")) := NULL]


# Features engineering ----------------------------------------------------


# We drop abandonned games
data_clean <- data_clean[!grep("RET", score),]
data_clean <- data_clean[!grep("W/O", score),]

# We have to do something with the "score" variable
data_clean[, winner_score := str_extract_all(score, "\\d+(?=\\-)")]
data_clean[, loser_score := str_extract_all(score, "(?<=-)\\d+")]

# I do replace missing set by 0 instead of NA (ce sont les sets qui n'ont pas été joués)
for (set in 1:5){
  data_clean[, paste0("winner_score_", set) := unlist(lapply(.I, function(i){
    ifelse(is.na(unlist(winner_score[i])[set]), 0, as.numeric(unlist(winner_score[i])[set]))
  }))]
  data_clean[, paste0("loser_score_", set) := unlist(lapply(.I, function(i){
    ifelse(is.na(unlist(loser_score[i])[set]), 0, as.numeric(unlist(loser_score[i])[set]))
  }))]
}
data_clean[, (c("winner_score", "loser_score", "score")) := NULL]

