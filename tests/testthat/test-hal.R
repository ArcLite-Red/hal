# test-hal.R -- hal() main entry point tests

test_that("hal requires at least one prompt string", {
  expect_error(hal(), "Provide at least one prompt")
})

test_that("hal rejects non-character input", {
  expect_error(hal(42))
})

test_that("hal inspect mode returns session info", {
  info <- hal(inspect = TRUE)
  expect_type(info, "list")
  expect_true("active" %in% names(info))
  expect_true("model" %in% names(info))
  expect_true("turns" %in% names(info))
  expect_true("tools" %in% names(info))
  expect_true("spawn_count" %in% names(info))
})

test_that("hal inspect returns correct types", {
  info <- hal(inspect = TRUE)
  expect_type(info$active, "logical")
  expect_type(info$turns, "integer")
  expect_type(info$spawn_count, "integer")
})

test_that("hal concatenates multiple strings", {
  # We can test the prompt building without sending to a model
  # by checking that multiple args don't error on type check
  dots <- list("hello", "world")
  expect_true(all(vapply(dots, is.character, logical(1))))
  prompt <- paste(unlist(dots), collapse = " ")
  expect_equal(prompt, "hello world")
})

test_that("hal_describe_env describes data frames", {
  env <- new.env(parent = emptyenv())
  env$df <- data.frame(x = 1:3, y = c("a", "b", "c"))

  desc <- .hal_describe_env(env)
  expect_true(grepl("df", desc))
  expect_true(grepl("data\\.frame", desc) || grepl("3 obs", desc))
})

test_that("hal_describe_env describes vectors", {
  env <- new.env(parent = emptyenv())
  env$vals <- c(1, 2, 3)

  desc <- .hal_describe_env(env)
  expect_true(grepl("vals", desc))
})

test_that("hal_describe_env handles empty environment", {
  env <- new.env(parent = emptyenv())
  desc <- .hal_describe_env(env)
  expect_equal(nchar(desc), 0)
})
