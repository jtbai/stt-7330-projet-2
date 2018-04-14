library(testthat)
source("modeling/hyper-parameter-selection.R")

A_NUMBER_OF_HYPER_PARAMETER_TO_TEST = 4
A_NUMBER_OF_KFOLD = 3

context("Test kfold functions")

test_that("when model get_calculation_jobs with N hyper_parameters and M kfold then N*M correct jobs are returned",{
  jobs_to_do = get_calculations_to_do(A_NUMBER_OF_HYPER_PARAMETER_TO_TEST, A_NUMBER_OF_KFOLD)
  
  expected_number_of_rows = A_NUMBER_OF_HYPER_PARAMETER_TO_TEST * A_NUMBER_OF_KFOLD
  expect_equal(expected = expected_number_of_rows, nrow(jobs_to_do))
})
