# -------------------------------------------------------------------------
# Title: dim-reduction
# Goal: vizulise-data
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(ggplot2)
library(ggthemes)


# Load data ---------------------------------------------------------------

source("data/scripts/clean-data.R")
source("data/scripts/preprocess-data.R")

# Multidimensional scaling ------------------------------------------------

sample_viz <- sample(1:nrow(data_imputed), nrow(data_imputed) * 0.05)
grps <- data_imputed[sample_viz, surface]
mds <- cmdscale(dist(scale(data_imputed[sample_viz, -(c("surface")), with = F]), method = "euclidean"), k = 2)

data_plot <- data.table(
  PC1 = mds[, 1],
  PC2 = mds[, 2],
  group = grps
)

ggplot(data_plot, aes(x = PC1, y = PC2, color = as.factor(grps))) +
  geom_point() +
  scale_color_discrete("Groupe") +
  scale_x_continuous("Première dimension projetée") +
  scale_y_continuous("Deuxième dimension projetée") +
  theme_classic()
