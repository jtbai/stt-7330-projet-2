# -------------------------------------------------------------------------
# Title: test-models
# Goal: Test few models
# Date: April 2018
# Author: Stéphane Caron
# -------------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(e1071)
library(randomForest)

# Import data -------------------------------------------------------------

source("data/scripts/clean-data.R")
source("data/scripts/preprocess-data.R")

# Model -------------------------------------------------------------------

test_index <- sample(1:nrow(data_imputed), 0.2 * nrow(data_imputed))
data_train <- data_imputed[-test_index,]
data_test <- data_imputed[test_index,]

model = naiveBayes(surface ~ ., data = data_train)
out = predict(model, data_test)
sum(out == data_test[, surface]) / nrow(data_test)


model_rf <- randomForest(surface ~ ., data = data_train)
out = predict(model_rf, data_test)
sum(out != data_test[, surface]) / nrow(data_test)
