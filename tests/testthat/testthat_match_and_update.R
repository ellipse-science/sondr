test_that("match_and_update works", {
  main <- c(a = 1, b = 2, c = 3, d = 4)
  updates <- c(b = 22, c = 33)
  result <- match_and_update(main, updates)
  expect_equal(result, c(a = 1, b = 22, c = 33, d = 4))

  # Test with empty updates
  result2 <- match_and_update(main, c())
  expect_equal(result2, main)

  # Test with updates not matching main
  result3 <- match_and_update(main, c(e = 5, f = 6))
  expect_equal(result3, main)

  # Test with no names
  result4 <- match_and_update(c(1, 2, 3, 4), c(22, 33))
  expect_equal(result4, c(1, 2, 3, 4))
})
