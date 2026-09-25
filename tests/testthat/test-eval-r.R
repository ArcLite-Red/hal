# Tests for hal eval_r tool

test_that("eval_r executes simple code", {
  # Set up a temporary eval env
  session <- .hal_get_session()
  session$eval_caller_env <- new.env(parent = globalenv())
  session$eval_r_assigned <- character()

  result <- .hal_eval_r("1 + 1")
  expect_match(result, "2")
})

test_that("eval_r blocks denylisted calls", {
  session <- .hal_get_session()
  session$eval_caller_env <- new.env(parent = globalenv())

  result <- .hal_eval_r("system('ls')")
  expect_match(result, "Blocked")
})

test_that("eval_r detects new assignments", {
  session <- .hal_get_session()
  test_env <- new.env(parent = globalenv())
  session$eval_caller_env <- test_env
  session$eval_r_assigned <- character()

  .hal_eval_r("my_var <- 42")
  expect_true("my_var" %in% session$eval_r_assigned)
  expect_equal(get("my_var", envir = test_env), 42)
})

test_that("eval_r handles errors gracefully", {
  session <- .hal_get_session()
  session$eval_caller_env <- new.env(parent = globalenv())

  result <- .hal_eval_r("stop('test error')")
  expect_match(result, "Error")
  expect_match(result, "test error")
})

test_that("eval_r tool def has correct schema", {
  tool <- .hal_eval_r_tool_def()
  expect_equal(tool$name, "eval_r")
  expect_true(is.function(tool$fun))
  expect_equal(tool$parameters$type, "object")
  expect_true("code" %in% names(tool$parameters$properties))
})

test_that("strip_fences removes markdown code fences", {
  input <- "```r\nx <- 1\ny <- 2\n```"
  result <- .hal_strip_fences(input)
  expect_equal(result, c("x <- 1", "y <- 2"))
})

test_that("strip_fences handles plain text", {
  result <- .hal_strip_fences("x <- 1")
  expect_equal(result, "x <- 1")
})

# ------------------------------------------------------------------------------
# Plot vision -- eval_r returns images when a plot is drawn
# ------------------------------------------------------------------------------

test_that(".hal_eval_r returns hal_tool_result with image when code plots", {
  skip_if_not(capabilities("png"))
  local_recordable_device()
  withr::local_options(list(hal.backend = "claude", hal.plot_vision = TRUE,
                            hal.session_quiet = TRUE))
  res <- .hal_eval_r("plot(1:10)")
  expect_s3_class(res, "hal_tool_result")
  expect_type(res$text, "character")
  expect_match(res$text, "## Plot: rendered and attached")
  expect_identical(res$image$mimeType, "image/png")
  bytes <- jsonlite::base64_dec(res$image$data)
  expect_identical(as.integer(bytes[1:4]), c(137L, 80L, 78L, 71L))
})

test_that(".hal_eval_r stays a plain string when vision is off", {
  local_recordable_device()
  withr::local_options(list(hal.backend = "claude", hal.plot_vision = FALSE))
  res <- .hal_eval_r("plot(1:10)")
  expect_type(res, "character")
  expect_false(inherits(res, "hal_tool_result"))
})

test_that(".hal_eval_r stays a plain string on copilot backend", {
  local_recordable_device()
  withr::local_options(list(hal.backend = "copilot", hal.plot_vision = TRUE))
  res <- .hal_eval_r("plot(1:10)")
  expect_type(res, "character")
  expect_false(inherits(res, "hal_tool_result"))
})

test_that("text truncation never touches the image payload", {
  skip_if_not(capabilities("png"))
  local_recordable_device()
  withr::local_options(list(hal.backend = "claude", hal.plot_vision = TRUE,
                            hal.session_quiet = TRUE))
  # Long printed output (>8000 chars) plus a plot in the same eval
  res <- .hal_eval_r("print(paste(rep('x', 6000), collapse = ' ')); plot(1:10)")
  expect_s3_class(res, "hal_tool_result")
  expect_match(res$text, "truncated")
  bytes <- jsonlite::base64_dec(res$image$data)
  expect_identical(as.integer(bytes[1:4]), c(137L, 80L, 78L, 71L))
})

test_that(".hal_ipc_eval_fn returns image field when code plots", {
  skip_if_not(capabilities("png"))
  local_recordable_device()
  withr::local_options(list(hal.backend = "claude", hal.plot_vision = TRUE,
                            hal.session_quiet = TRUE))
  res <- .hal_ipc_eval_fn("plot(1:10)")
  expect_null(res$error)
  expect_match(res$result, "## Plot: rendered and attached")
  expect_identical(res$image$mimeType, "image/png")
})

test_that(".hal_ipc_eval_fn omits image when nothing plotted", {
  withr::local_options(list(hal.backend = "claude", hal.plot_vision = TRUE))
  res <- .hal_ipc_eval_fn("1 + 1")
  expect_null(res$error)
  expect_null(res$image)
})
