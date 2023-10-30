library(testthat)

test_that("parse_money_range works correctly", {
  expect_equal(parse_money_range("$1,000 to $1,999", sep = "to"), c(1000, 1999))
  expect_equal(parse_money_range("under $1,000", limit = "floor"), c(0, 1000))
  expect_equal(parse_money_range("$15,000 or over", limit = "ceiling"), c(15000, 25000))
  expect_equal(parse_money_range("$20,000 or over", limit = "ceiling", ceiling_increment = 30000), c(20000, 50000))
  expect_error(parse_money_range("$1,000 to $1,999"), "sep must be provided if limit is NULL")
  expect_error(parse_money_range("$1,000 to $1,999", sep = "to", limit = "invalid"), "Invalid limit. Must be 'floor', 'ceiling', or NULL.")
})
