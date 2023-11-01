library(testthat)

test_that("extract_elements_with_prefix works correctly", {
  vec <- c(ces79a = 1, ces79b = 2, other = 3, ces79c = 4)
  result <- extract_elements_with_prefix(vec, "ces79")

  expect_equal(result, c(ces79a = 1, ces79b = 2, ces79c = 4))

  result2 <- extract_elements_with_prefix(vec, "other")
  expect_equal(result2, c(other = 3))
})
