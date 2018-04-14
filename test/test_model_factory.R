library(testthat)
source("modeling/model-factory.R")


context("Test factory")

test_that("when model factory called with random_forest then randomforest object is return",{
  random_forest_model = get_model_function("random_forest")
  
  method_used = methods(random_forest_model)[1]
  expect_equal(expected = "randomForest.default", method_used)
})

test_that("when model factory called with tree_based then rpart object is return",{
  tree_model = get_model_function("tree_based")
  
  package = getAnywhere(rpart)$where[1]
  expect_equal(expected = "package:rpart", package) 
})
