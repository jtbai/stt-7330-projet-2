# -------------------------------------------------------------------------
# Title: stat-desc
# Goal: This script aims to output simple statistics from dataset
# Date: April 2018
# Author: Christopher Blier-Wong
# -------------------------------------------------------------------------

library(data.table)

data_all <- fread("data/data_modeling_classical_methods.csv")

data_all$split_group <- NULL

boxplot(ace_by_svpt ~ surface, data = data_all, main = "Aces par services", names = c("Hard", "Clay", "Grass"))
boxplot(first_won_by_first_in ~ surface, data = data_all)
boxplot(first_won_by_serve_won ~ surface, data = data_all)

boxplot(min_by_svpt ~ surface, data = data_all)
boxplot(nb_tie_break ~ surface, data = data_all)

aggregate(data_all[, -1], by = list(data_all$surface), 
          FUN = function(x) {c(min = min(x), moy = mean(x), max = max(x), med = median(x), var = var(x))})

# plot correlations -------------------------------------------------------

library(corrplot)
corrplot(cor(data_all), method="circle")

# stats sur surface -------------------------------------------------------

table(data_all$surface)

# variables individuelles -------------------------------------------------

hist(data_all$ace_by_svpt)

par(mfrow = c(1, 3))
hist(data_all[surface == 1]$ace_by_svpt, main = "Hard", xlim = c(0, 0.4), xlab = "ace_by_svpt")
hist(data_all[surface == 2]$ace_by_svpt, main = "Clay", xlim = c(0, 0.4), xlab = "ace_by_svpt")
hist(data_all[surface == 3]$ace_by_svpt, main = "Grass", xlim = c(0, 0.4), xlab = "ace_by_svpt")

