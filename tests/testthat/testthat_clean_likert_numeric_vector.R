library(sondr)

test_that("clean_likert_numeric_vector works with default parameters", {
  # Test with a 5-point Likert scale
  raw_data <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
  expected <- c(0, 0.25, 0.5, 0.75, 1, 0, 0.25, 0.5, 0.75, 1)
  
  result <- clean_likert_numeric_vector(raw_data)
  expect_equal(result, expected)
  
  # Test with a 7-point Likert scale
  raw_data2 <- c(1, 2, 3, 4, 5, 6, 7)
  expected2 <- c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 1)
  
  result2 <- clean_likert_numeric_vector(raw_data2)
  expect_equal(result2, expected2)
})

test_that("clean_likert_numeric_vector works with revert=TRUE", {
  # Test with a 5-point Likert scale
  raw_data <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
  expected <- c(1, 0.75, 0.5, 0.25, 0, 1, 0.75, 0.5, 0.25, 0)
  
  result <- clean_likert_numeric_vector(raw_data, revert = TRUE)
  expect_equal(result, expected)
  
  # Test with a 7-point Likert scale
  raw_data2 <- c(1, 2, 3, 4, 5, 6, 7)
  expected2 <- c(1, 5/6, 4/6, 3/6, 2/6, 1/6, 0)
  
  result2 <- clean_likert_numeric_vector(raw_data2, revert = TRUE)
  expect_equal(result2, expected2)
})