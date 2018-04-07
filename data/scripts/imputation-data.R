# -------------------------------------------------------------------------
# Title: imputation-data
# Goal: This script is aimed to do missing data imputation
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Do missing data imputations ---------------------------------------------

do_missing_data_imputation <- function(dt){
  
  data_imputed <- dt %>% drop_na()
  
}


