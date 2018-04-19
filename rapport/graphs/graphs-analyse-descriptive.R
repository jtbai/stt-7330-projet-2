# -------------------------------------------------------------------------
# Title: graphs-analyse-descriptive
# Goal: Build graphs for descriptive analysis
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(data.table)
library(ggplot2)


# Import data -------------------------------------------------------------

source("data/scripts/data-factory.R")


str(data_raw) 
str(data_modeling_classical_methods)

correlation_matrix <- cor(data_modeling_classical_methods)
corrplot::corrplot(correlation_matrix)

