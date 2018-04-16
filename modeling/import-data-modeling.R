# -------------------------------------------------------------------------
# Title: import-data-modeling.R
# Goal: Import data for modeling and fix formats
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

import_data_modeling <- function(path, selected_group) {
  
  data_modeling_classical_methods <- fread(path)
  data_modeling_classical_methods[, surface := as.factor(surface)]
  
  data <- data_modeling_classical_methods[split_group %in% selected_group, -(c("split_group")), with = FALSE]
  
  return(data)
  
}
