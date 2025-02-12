library(testthat)

test_that("rename_snakecase correctly renames columns", {
  df <- data.frame(FirstName = c("John", "Jane"), LastName = c("Doe", "Smith"))
  result <- rename_snakecase(df)
  
  expect_equal(colnames(result), c("first_name", "last_name"))
})

test_that("rename_snakecase handles already snake_case columns", {
  df <- data.frame(first_name = c("John", "Jane"), last_name = c("Doe", "Smith"))
  result <- rename_snakecase(df)
  
  expect_equal(colnames(result), c("first_name", "last_name"))
})

#test_that("rename_snakecase handles mixed case column names", {
#  df <- data.frame(FirstNAME = c("John", "Jane"), LASTName = c("Doe", "Smith"))
#  result <- rename_snakecase(df)
#  
#  expect_equal(colnames(result), c("first_name", "last_name"))
#})

test_that("rename_snakecase works with single column data frames", {
  df <- data.frame(SingleColumn = c(1, 2, 3))
  result <- rename_snakecase(df)
  
  expect_equal(colnames(result), "single_column")
})

test_that("rename_snakecase works with no columns", {
  df <- data.frame()
  result <- rename_snakecase(df)
  
  expect_equal(colnames(result), character(0))
})

test_that("rename_snakecase works within a dplyr pipeline", {
  df <- data.frame(FirstName = c("John", "Jane"), LastName = c("Doe", "Smith"))
  result <- df |> rename_snakecase() |> dplyr::mutate(full_name = paste(first_name, last_name))
  
  expect_true("full_name" %in% colnames(result))
  expect_equal(result$full_name, c("John Doe", "Jane Smith"))
})
