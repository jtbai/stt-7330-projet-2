library(data.table)

data_for_statistaks <- fread("data/data_modeling_classical_methods.csv")

data_for_statistaks$split_group <- NULL

boxplot(ace_by_svpt ~ surface, data = data_for_statistaks)
boxplot(first_won_by_first_in ~ surface, data = data_for_statistaks)

# plot correlations -------------------------------------------------------

library(corrplot)
corrplot(cor(data_for_statistaks), method="circle")

# variables individuelles -------------------------------------------------

hist(data_for_statistaks$ace_by_svpt)

