# -------------------------------------------------------------------------
# Title: import-data
# Goal: Import tennis data (by game) from the web
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# For this projet, we will use data from JeffSackmann repo (https://github.com/JeffSackmann/tennis_atp).
# This repo contains ATP game-by-game data from 1968 to up to now.

import_games <- function(year_from, year_to){
  
  list_data <- lapply(year_from:year_to, function(year){
    temp <- getURL(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_", year, ".csv"))
    fread(temp, header = TRUE, sep = ",")
  })
  
  rbindlist(list_data, use.names = TRUE, fill = FALSE)
}

