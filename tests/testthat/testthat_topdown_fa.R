library(testthat)
library(psych) # For alpha function
library(ggplot2) # For ggplot object testing

context("Testing topdown_fa function")

# Test if the function accepts only dataframes
test_that("Function accepts a dataframe", {
  expect_error(topdown_fa("not_a_dataframe"), "Error: Argument 'df' must be a dataframe")
})

# Test if the function calculates Cronbach's alpha and the first eigenvalue correctly
test_that("Correct calculation of Cronbach's alpha and first eigenvalue", {
  test_df <- data.frame(matrix(rnorm(100), ncol=5))
  test_result <- topdown_fa(test_df, nfactors = 1)
  
  # Expected values
  expected_alpha <- round(psych::alpha(test_df)$total$raw_alpha, 2)
  expected_eigen <- round(eigen(cor(test_df))$values[1], 2)
  
  # Check if the calculated values match the expected values
  expect_equal(test_result$cronbachAlpha, expected_alpha)
  expect_equal(test_result$factor1stEigen, expected_eigen)
})

# Test if the function returns a list with expected components
test_that("Return value contains expected components", {
  test_df <- data.frame(matrix(rnorm(100), ncol=5))
  test_result <- topdown_fa(test_df, nfactors = 1)
  
  # Check for list and its components
  expect_true(is.list(test_result))
  expect_true(all(c("cronbachAlpha", "factor1stEigen", "factorLoadings") %in% names(test_result)))
})

# Test if the function returns a ggplot object
test_that("Function returns a ggplot object", {
  test_df <- data.frame(matrix(rnorm(100), ncol=5))
  test_result <- topdown_fa(test_df, nfactors = 1)
  
  # Check if the result is a ggplot object
  expect_true(is.ggplot(test_result$FAplot))
})

# You may need to add more tests depending on the expected behavior of your function.


