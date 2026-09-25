# Tests for hal output formatting

test_that("format_response handles headers", {
  formatted <- .hal_format_response("# Title\n\nSome text")
  expect_true(nzchar(formatted))
  expect_match(formatted, "(?i)Title", perl = TRUE)
})

test_that("format_response handles code blocks", {
  text <- "Here is code:\n```r\nx <- 1\n```\nDone."
  formatted <- .hal_format_response(text)
  expect_match(formatted, "x <- 1")
})

test_that("format_response handles bullet lists", {
  text <- "Items:\n- First\n- Second\n- Third"
  formatted <- .hal_format_response(text)
  expect_match(formatted, "First")
  expect_match(formatted, "Third")
})

test_that("format_response handles numbered lists", {
  text <- "Steps:\n1. First\n2. Second"
  formatted <- .hal_format_response(text)
  expect_match(formatted, "First")
})

test_that("format_response handles bold text", {
  text <- "This is **important** text"
  formatted <- .hal_format_response(text)
  expect_match(formatted, "important")
})

test_that("format_response handles inline code", {
  text <- "Use the `print()` function"
  formatted <- .hal_format_response(text)
  expect_match(formatted, "print")
})

test_that("parse_chunks splits on structural elements", {
  text <- "# Header\ntext\n- item\nmore text"
  chunks <- .hal_parse_chunks(text)
  expect_true(length(chunks) >= 3)
})

test_that("supports_color respects option", {
  withr::with_options(list(hal.use_colors = FALSE), {
    expect_false(.hal_supports_color())
  })
})

test_that("display_response works with instant speed", {
  expect_output(
    .hal_display_response("Hello world", stream = FALSE),
    "Hello world"
  )
})
