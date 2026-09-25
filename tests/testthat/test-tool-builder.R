# Tests for hal tool builder

test_that("hal_tool builds from function with defaults", {
  add <- function(x = 0, y = 0) x + y
  tool <- hal_tool(add, "add_numbers", description = "Add two numbers")

  expect_equal(tool$name, "add_numbers")
  expect_equal(tool$description, "Add two numbers")
  expect_true(is.function(tool$fun))
  expect_equal(tool$parameters$type, "object")
  expect_true("x" %in% names(tool$parameters$properties))
  expect_true("y" %in% names(tool$parameters$properties))
})

test_that("hal_tool infers types from defaults", {
  fn <- function(name, count = 5L, ratio = 0.5, flag = TRUE) NULL
  tool <- suppressWarnings(hal_tool(fn, "test_fn"))  # warns about 'name'

  props <- tool$parameters$properties
  expect_equal(props$name$type, "string")    # no default = string

  expect_equal(props$count$type, "integer")
  expect_equal(props$ratio$type, "number")
  expect_equal(props$flag$type, "boolean")
})

test_that("hal_tool respects type overrides", {
  fn <- function(weight, height) NULL
  tool <- hal_tool(fn, "body_stats",
    types = list(weight = "number", height = "number")
  )

  props <- tool$parameters$properties
  expect_equal(props$weight$type, "number")
  expect_equal(props$height$type, "number")
})

test_that("hal_tool marks required params", {
  fn <- function(required_arg, optional = "default") NULL
  tool <- suppressWarnings(hal_tool(fn, "test_required"))  # warns about required_arg

  expect_true("required_arg" %in% unlist(tool$parameters$required))
  expect_false("optional" %in% unlist(tool$parameters$required))
})

test_that("hal_tool warns about untyped required params", {
  fn <- function(x, y) x + y
  expect_warning(
    hal_tool(fn, "warn_test"),
    "typed as string"
  )
})

test_that("hal_tool handles zero-arg functions", {
  fn <- function() Sys.time()
  tool <- hal_tool(fn, "get_time", description = "Get current time")

  expect_equal(tool$parameters$type, "object")
  expect_length(tool$parameters$properties, 0)
})

test_that(".hal_infer_type handles edge cases", {
  expect_equal(.hal_infer_type(NULL), "string")
  expect_equal(.hal_infer_type(quote(expr = )), "string")
  expect_equal(.hal_infer_type(quote(42)), "number")
  expect_equal(.hal_infer_type(quote(42L)), "integer")
  expect_equal(.hal_infer_type(quote(TRUE)), "boolean")
  expect_equal(.hal_infer_type(quote("hello")), "string")
})
