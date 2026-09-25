test_that("compact() removes NULL elements", {
  x <- list(a = 1, b = NULL, c = "x")
  expect_equal(compact(x), list(a = 1, c = "x"))
})

test_that("compact() returns empty list for all NULLs", {
  x <- list(a = NULL, b = NULL)
  expect_length(compact(x), 0)
})

test_that("compact() preserves non-NULL values", {
  x <- list(a = 1, b = 2)
  expect_equal(compact(x), list(a = 1, b = 2))
})
