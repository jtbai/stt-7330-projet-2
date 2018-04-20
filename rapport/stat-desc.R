library(data.table)

data_all <- fread("data/data_modeling_classical_methods.csv")

data_all$split_group <- NULL

boxplot(ace_by_svpt ~ surface, data = data_all)
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
hist(data_all$min_by_svpt)
