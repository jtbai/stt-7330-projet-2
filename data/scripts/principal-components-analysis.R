# -------------------------------------------------------------------------
# Title: principal-components-anlaysis
# Goal: This script aims to do PCA on the data
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------

do_pca <- function(dt, cumulative_variance_to_keep = 0.8) {
  
  data_without_response <- copy(dt)[, -(c("surface", "split_group")), with = FALSE]
  
  pca <- prcomp(data_without_response, scale. = TRUE)
  nb_components_to_keep <- which(cumsum(pca$sdev^2/sum(pca$sdev^2)) >= cumulative_variance_to_keep)[1]
  
  data_reduced <- data.table(cbind(dt$surface, dt$split_group, pca$x[, 1:nb_components_to_keep]))
  setnames(data_reduced, c("V1", "V2"), c("surface", "split_group"))
  
  return(data_reduced)
  
}