test_that("as_tool_def() handles plain list format", {
  td <- as_tool_def(list(
    name = "add",
    description = "Add two numbers",
    fun = function(a, b) a + b,
    parameters = list(type = "object", properties = list())
  ))

  expect_equal(td$name, "add")
  expect_equal(td$description, "Add two numbers")
  expect_true(is.function(td$fun))
  expect_equal(td$schema$type, "function")
  expect_equal(td$schema$`function`$name, "add")
})

test_that("as_tool_def() errors on missing name", {
  expect_error(
    as_tool_def(list(fun = identity)),
    "name"
  )
})

test_that("as_tool_def() errors on missing fun", {
  expect_error(
    as_tool_def(list(name = "test")),
    "fun"
  )
})

test_that("tool_result_text passes through character scalars, JSON-encodes the rest", {
  # Already-formatted text (e.g. a markdown report) is untouched.
  expect_equal(tool_result_text("# Report\n- ok"), "# Report\n- ok")
  expect_equal(tool_result_text(NULL), "(no output)")

  # A data frame becomes row-oriented JSON, not column-deparse noise.
  df <- data.frame(id = c("a", "b"), n = c(1L, 2L), stringsAsFactors = FALSE)
  js <- tool_result_text(df)
  expect_match(js, '^\\[\\{', perl = TRUE)              # JSON array of objects
  parsed <- jsonlite::fromJSON(js)
  expect_equal(parsed$id, c("a", "b"))
  expect_equal(parsed$n, c(1L, 2L))

  # A nested list (e.g. apply_fixes: {result, applied=<df>}) round-trips.
  out <- list(result = "tl_fixed", applied = df)
  parsed2 <- jsonlite::fromJSON(tool_result_text(out))
  expect_equal(parsed2$result, "tl_fixed")
  expect_equal(nrow(parsed2$applied), 2L)
})

test_that("as_tool_def() ingests an ellmer ToolDef (>= 0.4 S7 class)", {
  skip_if_not_installed("ellmer")

  td <- ellmer::tool(
    function(data, fix_ids, n = 1L) data,
    description = "test tool",
    arguments = list(
      data = ellmer::type_string("data name"),
      fix_ids = ellmer::type_array(items = ellmer::type_string(), description = "ids"),
      n = ellmer::type_integer("count", required = FALSE)
    ),
    name = "test_tool"
  )

  out <- as_tool_def(td)
  expect_equal(out$name, "test_tool")
  expect_true(is.function(out$fun))
  expect_equal(out$fun(data = "x"), "x")

  params <- out$schema[["function"]]$parameters
  expect_equal(params$type, "object")
  # Types must survive conversion, not collapse to "string".
  expect_equal(params$properties$fix_ids$type, "array")
  expect_equal(params$properties$fix_ids$items$type, "string")
  expect_equal(params$properties$n$type, "integer")
  # Only arguments without defaults are required.
  expect_setequal(unlist(params$required), c("data", "fix_ids"))
})
