# -------------------------------------------------------------------------
# Title: import-data-modeling.R
# Goal: Import data for modeling and fix formats
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

import_data_modeling <- function(path, ind_train, ind_test) {
  
  data_modeling_classical_methods <- fread(path)
  data_modeling_classical_methods[, surface := as.factor(surface)]
  
  data_train <- data_modeling_classical_methods[split_group %in% ind_train, -(c("split_group")), with = FALSE]
  data_test <- data_modeling_classical_methods[split_group %in% ind_test, -(c("split_group")), with = FALSE]
  
  list(
    data_train = data_train,
    data_test = data_test
  )
  
}
