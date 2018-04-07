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


# Create datasets ---------------------------------------------------------

data_raw <- import_games(year_from = year_from, year_to = year_to)
data_clean <- clean_data_from_raw(dt = data_raw)

