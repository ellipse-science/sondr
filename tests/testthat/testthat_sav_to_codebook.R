library(testthat)

# Sample data for testing
sample_data <- data.frame(
  Var1 = 1:3,
  Var2 = c("A", "B", "C")
)
attr(sample_data$Var1, "label") <- "This is a numeric variable"
attr(sample_data$Var2, "label") <- "This is a factor variable"
attr(sample_data$Var2, "labels") <- c(A = 1, B = 2, C = 3)

test_that("sav_to_codebook returns a data frame", {
  codebook <- sav_to_codebook(sample_data)
  expect_true(is.data.frame(codebook))
})

test_that("sav_to_codebook returns correct structure", {
  codebook <- sav_to_codebook(sample_data)
  expected_names <- c("variable_name", "question", "answers")
  expect_equal(names(codebook), expected_names)
})

test_that("sav_to_codebook handles data with attributes", {
  codebook <- sav_to_codebook(sample_data)
  expect_equal(codebook$question[1], "This is a numeric variable")
  expect_equal(codebook$answers[2], "A: 1; B: 2; C: 3")
})

test_that("sav_to_codebook handles data without attributes", {
  # Data without attributes
  plain_data <- data.frame(Var1 = 1:3, Var2 = c("D", "E", "F"))
  codebook <- sav_to_codebook(plain_data)
  
  expect_true(all(is.na(codebook$question)))
  expect_true(all(is.na(codebook$answers)))
})

