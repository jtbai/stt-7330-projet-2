library(data.table)

data_for_statistaks <- fread("data/data_modeling_classical_methods.csv")

data_for_statistaks$split_group <- NULL

boxplot(ace_by_svpt ~ surface, data = data_for_statistaks)
boxplot(first_won_by_first_in ~ surface, data = data_for_statistaks)
boxplot(first_won_by_serve_won ~ surface, data = data_for_statistaks)

boxplot(min_by_svpt ~ surface, data = data_for_statistaks)
boxplot(nb_tie_break ~ surface, data = data_for_statistaks)


aggregate(data_for_statistaks, by = list(data_for_statistaks$surface), 
          FUN = function(x) {c(min = min(x), moy = mean(x), max = max(x), med = median(x), var = var(x))})

# plot correlations -------------------------------------------------------

library(corrplot)
corrplot(cor(data_for_statistaks), method="circle")


# stats sur surface -------------------------------------------------------

table(data_for_statistaks$surface)

# variables individuelles -------------------------------------------------


hist(data_for_statistaks$ace_by_svpt)
hist(data_for_statistaks$min_by_svpt)
hist(data_for_statistaks$ace_by_svpt)

