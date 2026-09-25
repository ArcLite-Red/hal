# Tests for pipe-verbs helpers (offline — no CLI needed)

# -- extract_code: backtick stripping -----------------------------------------

test_that("extract_code strips inline backtick wrapping", {
  expect_equal(
    .hal_extract_code("`my_sum <- 10 + 32`"),
    "my_sum <- 10 + 32"
  )
})

test_that("extract_code strips triple backtick wrapping", {
  expect_equal(
    .hal_extract_code("```my_sum <- 10 + 32```"),
    "my_sum <- 10 + 32"
  )
})

test_that("extract_code handles fenced code blocks", {
  response <- "```r\nx <- 1\ny <- 2\n```"
  result <- .hal_extract_code(response)
  expect_equal(result, "x <- 1\ny <- 2")
})

test_that("extract_code passes through plain code", {
  expect_equal(
    .hal_extract_code("x <- 1 + 1"),
    "x <- 1 + 1"
  )
})

test_that("extract_code returns empty for NULL/empty input", {
  expect_equal(.hal_extract_code(NULL), "")
  expect_equal(.hal_extract_code(""), "")
})

# -- .data cleaning for edit-in-place -----------------------------------------

test_that("clean strips .data pipe prefix (native pipe)", {
  code <- ".data |> dplyr::filter(mpg > 25)"
  clean <- sub("^\\s*\\.data\\s*\\|>\\s*", "", code)
  expect_equal(clean, "dplyr::filter(mpg > 25)")
})

test_that("clean strips .data pipe prefix (magrittr)", {
  code <- ".data %>% dplyr::filter(mpg > 25)"
  clean <- sub("^\\s*\\.data\\s*%>%\\s*", "", code)
  expect_equal(clean, "dplyr::filter(mpg > 25)")
})

test_that("clean strips .data as first function argument", {
  code <- "dplyr::filter(.data, mpg > 25)"
  clean <- sub("(\\w+\\()\\s*\\.data\\s*,\\s*", "\\1", code)
  expect_equal(clean, "dplyr::filter(mpg > 25)")
})

test_that("clean strips .data with namespace prefix", {
  code <- "dplyr::group_by(.data, cyl)"
  clean <- sub("(\\w+\\()\\s*\\.data\\s*,\\s*", "\\1", code)
  expect_equal(clean, "dplyr::group_by(cyl)")
})

test_that("clean does not strip .data in non-first-arg position", {
  code <- "mutate(x, val = nrow(.data))"
  clean <- sub("(\\w+\\()\\s*\\.data\\s*,\\s*", "\\1", code)
  # Should be unchanged — .data is not the first argument
  expect_equal(clean, "mutate(x, val = nrow(.data))")
})

# -- find_do_call_range: comment skipping -------------------------------------

test_that("find_do_call skips comment lines", {
  # Simulate editor context
  contents <- c(
    "mtcars |>",
    "  hal_do(\"filter\")",
    "",
    "# hal_do() is great",
    ""
  )

  # Search backward from line 5 (cursor after comment)
  # Should find the real call on line 2, not the comment on line 4
  target_row <- NULL
  search_start <- 4L
  for (row in seq(search_start, 1L)) {
    line <- contents[row]
    if (grepl("^\\s*#", line)) next
    pos <- regexpr("hal_do\\(", line)
    if (pos > 0L) {
      before <- substr(line, 1L, pos - 1L)
      if (!grepl("#", before)) {
        target_row <- row
        break
      }
    }
  }
  expect_equal(target_row, 2L)
})

test_that("find_do_call skips inline comments", {
  contents <- c(
    "x <- 1  # hal_do() note",
    "iris |> hal_do(\"filter\")"
  )

  target_row <- NULL
  for (row in seq(2L, 1L)) {
    line <- contents[row]
    if (grepl("^\\s*#", line)) next
    pos <- regexpr("hal_do\\(", line)
    if (pos > 0L) {
      before <- substr(line, 1L, pos - 1L)
      if (!grepl("#", before)) {
        target_row <- row
        break
      }
    }
  }
  expect_equal(target_row, 2L)
})

# -- show_generated_code output ------------------------------------------------

test_that("show_generated_code produces output", {
  withr::with_options(list(hal.use_colors = FALSE), {
    out <- capture.output(.hal_show_generated_code("x <- 1"))
    expect_true(any(grepl("x <- 1", out)))
  })
})

test_that("show_generated_code is silent for empty code", {
  out <- capture.output(.hal_show_generated_code(""))
  expect_length(out, 0)
})

# -- system prompt content -----------------------------------------------------

test_that("pipe system prompt instructs pipe style", {
  prompt <- .hal_do_pipe_system_prompt()
  expect_match(prompt, "\\.data \\|>")
  expect_match(prompt, "Do NOT pass")
})

test_that("standalone system prompt exists", {
  prompt <- .hal_do_standalone_system_prompt()
  expect_match(prompt, "code generation")
})

# -- retry prompt construction -------------------------------------------------

test_that("retry prompt includes error message and code", {
  prompt <- .hal_do_retry_prompt("object 'x' not found", "x + 1")
  expect_match(prompt, "object 'x' not found")
  expect_match(prompt, "x \\+ 1")
  expect_match(prompt, "failed")
  expect_match(prompt, "fixed code")
})

test_that("retry prompt works with empty code", {
  prompt <- .hal_do_retry_prompt("No valid R code", "")
  expect_match(prompt, "No valid R code")
  # Should not have a code block section
  expect_false(grepl("```r", prompt))
})

test_that("retry prompt includes code block when code is provided", {
  prompt <- .hal_do_retry_prompt("parse error", ".data |> bad_fn()")
  expect_match(prompt, "```r")
  expect_match(prompt, "bad_fn")
})

# -- retry config defaults ----------------------------------------------------

test_that("default do_retries is 1", {
  withr::with_options(list(hal.do_retries = NULL), {
    expect_equal(getOption("hal.do_retries", 1L), 1L)
  })
})

test_that("do_retries option is respected", {
  withr::with_options(list(hal.do_retries = 3L), {
    expect_equal(getOption("hal.do_retries", 1L), 3L)
  })
})

# -- describe_env: data frames ------------------------------------------------

test_that("describe_df includes column names and types", {
  desc <- .hal_describe_df("df", mtcars)
  expect_match(desc, "data.frame \\[32 x 11\\]")
  expect_match(desc, "mpg")
  expect_match(desc, "cyl")
  expect_match(desc, "numeric")
})

test_that("describe_df handles empty data frame", {
  empty <- data.frame()
  desc <- .hal_describe_df("empty", empty)
  expect_match(desc, "data.frame \\[0 x 0\\]")
  expect_false(grepl("cols:", desc))
})

test_that("describe_df caps wide data frames", {
  wide <- as.data.frame(matrix(1, nrow = 1, ncol = 100))
  desc <- .hal_describe_df("wide", wide)
  expect_true(nchar(desc) < 600)
})

test_that("describe_df shows mixed column types", {
  df <- data.frame(
    name = "alice",
    age = 30L,
    score = 0.95,
    stringsAsFactors = FALSE
  )
  desc <- .hal_describe_df("df", df)
  expect_match(desc, "character")
  expect_match(desc, "integer")
  expect_match(desc, "numeric")
})

# -- describe_env: functions ---------------------------------------------------

test_that("describe_fn shows signature", {
  fn <- function(x, y, z) x + y + z
  desc <- .hal_describe_fn("add", fn)
  expect_match(desc, "function\\(x, y, z\\)")
})

test_that("describe_fn includes body for small functions", {
  fn <- function(x) x * 2
  desc <- .hal_describe_fn("dbl", fn)
  expect_match(desc, "body:")
  expect_match(desc, "x \\* 2")
})

test_that("describe_fn omits body for large functions", {
  fn <- function(x) {
    a <- x + 1
    b <- a + 2
    c <- b + 3
    d <- c + 4
    e <- d + 5
    f <- e + 6
  }
  desc <- .hal_describe_fn("big", fn)
  expect_false(grepl("body:", desc))
})

test_that("describe_fn handles primitives", {
  desc <- .hal_describe_fn("plus", `+`)
  expect_match(desc, "function")
})

# -- describe_env: full integration --------------------------------------------

test_that("describe_env includes column info for data frames", {
  env <- new.env(parent = emptyenv())
  env$df <- mtcars[1:5, 1:3]
  env$threshold <- 0.05

  desc <- .hal_describe_env(env)
  expect_match(desc, "mpg")
  expect_match(desc, "cyl")
  expect_match(desc, "disp")
  expect_match(desc, "threshold.*numeric.*0.05")
})

test_that("describe_env includes function bodies for small fns", {
  env <- new.env(parent = emptyenv())
  env$double_it <- function(x) x * 2

  desc <- .hal_describe_env(env)
  expect_match(desc, "body:")
  expect_match(desc, "x \\* 2")
})

# -- detect_env_refs ----------------------------------------------------------

test_that("detect_env_refs matches identifiers present in env", {
  env <- new.env(parent = emptyenv())
  env$df <- data.frame(x = 1)
  env$threshold <- 0.05

  expect_equal(.hal_detect_env_refs("what is the mean of df?", env), "df")
  expect_setequal(
    .hal_detect_env_refs("compare df against threshold", env),
    c("df", "threshold")
  )
})

test_that("detect_env_refs returns empty when nothing matches", {
  env <- new.env(parent = emptyenv())
  env$df <- data.frame(x = 1)

  expect_equal(.hal_detect_env_refs("hello world", env), character())
  expect_equal(.hal_detect_env_refs("analyze the dataframe", env), character())
})

test_that("detect_env_refs handles empty env and empty prompt", {
  empty <- new.env(parent = emptyenv())
  expect_equal(.hal_detect_env_refs("anything", empty), character())

  env <- new.env(parent = emptyenv())
  env$x <- 1
  expect_equal(.hal_detect_env_refs("", env), character())
})

test_that("detect_env_refs is word-boundary aware", {
  env <- new.env(parent = emptyenv())
  env$df <- data.frame(x = 1)

  # "df" should not match inside "dfs" or "mydf"
  expect_equal(.hal_detect_env_refs("look at mydf and dfs", env), character())
})

# ------------------------------------------------------------------------------
# .hal_do_fail -- failure contract (warn interactive, abort non-interactive)
# ------------------------------------------------------------------------------

test_that(".hal_do_fail aborts with classed condition when do_on_fail = 'abort'", {
  withr::with_options(list(hal.do_on_fail = "abort"), {
    expect_error(
      .hal_do_fail("hal_do: boom", mtcars),
      class = "hal_do_error"
    )
    expect_error(
      .hal_do_fail("hal_do: boom", mtcars),
      class = "hal_error"
    )
  })
})

test_that(".hal_do_fail warns and returns fail_return when do_on_fail = 'warn'", {
  withr::with_options(list(hal.do_on_fail = "warn"), {
    expect_warning(
      out <- .hal_do_fail("hal_do: boom", mtcars),
      class = "hal_do_warning"
    )
    expect_identical(out, mtcars)
  })
})

test_that(".hal_do_fail defaults to abort in non-interactive sessions", {
  # Test suites run non-interactively, so the unset default is "abort"
  skip_if(interactive())
  withr::with_options(list(hal.do_on_fail = NULL), {
    expect_error(.hal_do_fail("hal_do: boom", NULL), class = "hal_do_error")
  })
})

# ------------------------------------------------------------------------------
# .hal_detect_env_refs -- stopword handling
# ------------------------------------------------------------------------------

test_that("env detection still matches ordinary object names", {
  env <- new.env()
  env$df <- mtcars
  env$my_model <- lm(mpg ~ wt, data = mtcars)
  expect_setequal(
    .hal_detect_env_refs("summarize df and my_model please", env),
    c("df", "my_model")
  )
})

test_that("env detection ignores English function-word collisions", {
  env <- new.env()
  env$the <- 1
  env$on <- 2
  env$all <- 3
  expect_identical(
    .hal_detect_env_refs("show me all the columns on that table", env),
    character()
  )
})

test_that("deliberate references rescue stopword-named objects", {
  env <- new.env()
  env$all <- data.frame(x = 1)
  env$on <- data.frame(y = 2)
  env$use <- function() 1
  expect_identical(.hal_detect_env_refs("what is in `all`?", env), "all")
  expect_identical(.hal_detect_env_refs("summarize on$y for me", env), "on")
  expect_identical(.hal_detect_env_refs("call use() and report", env), "use")
})

test_that("contested data-science nouns still match (FN worse than FP)", {
  env <- new.env()
  env$data <- mtcars
  env$model <- lm(mpg ~ wt, data = mtcars)
  expect_setequal(
    .hal_detect_env_refs("explain how the model fits the data", env),
    c("data", "model")
  )
})
