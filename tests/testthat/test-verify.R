# .hal_verify_transform + hal_verification S3 -- report-only verification

test_that("filter is reported as a row delta, nothing else", {
  before <- mtcars
  after <- mtcars[mtcars$mpg > 20, ]
  v <- .hal_verify_transform(before, after)
  expect_s3_class(v, "hal_verification")
  expect_identical(v$nrow_before, 32L)
  expect_identical(v$nrow_after, nrow(after))
  expect_length(v$cols_added, 0)
  expect_length(v$cols_removed, 0)
  expect_length(v$class_changes, 0)
  expect_length(v$new_na_cols, 0)
  expect_false(v$identical_output)
  expect_false(v$zero_rows)
  expect_match(.hal_verify_oneliner(v), "32 -> \\d+ rows")
})

test_that("added and removed columns are reported", {
  before <- mtcars
  after <- mtcars
  after$kpl <- after$mpg * 0.425
  after$cyl <- NULL
  v <- .hal_verify_transform(before, after)
  expect_identical(v$cols_added, "kpl")
  expect_identical(v$cols_removed, "cyl")
  one <- .hal_verify_oneliner(v)
  expect_match(one, "\\+1 col \\(kpl\\)")
  expect_match(one, "-1 col \\(cyl\\)")
})

test_that("class changes on common columns are reported", {
  before <- data.frame(x = 1:3, f = factor(c("a", "b", "a")))
  after <- before
  after$f <- as.character(after$f)
  v <- .hal_verify_transform(before, after)
  expect_named(v$class_changes, "f")
  expect_identical(v$class_changes$f$before, "factor")
  expect_identical(v$class_changes$f$after, "character")
  expect_match(.hal_verify_oneliner(v), "class changed: f")
})

test_that("introduced NAs detected with equal and unequal nrow", {
  before <- data.frame(x = c("1", "2", "oops"), y = 1:3)
  # equal nrow: failed coercion introduces NA
  after_eq <- before
  after_eq$x <- suppressWarnings(as.numeric(after_eq$x))
  v <- .hal_verify_transform(before, after_eq)
  expect_identical(v$new_na_cols, "x")
  expect_identical(v$na_increase_cols, "x")
  expect_match(.hal_verify_oneliner(v), "new NAs: x\\(1\\)")

  # unequal nrow: new_na_cols still fires (0 -> >0 is row-count robust),
  # na_increase_cols does not
  after_ne <- after_eq[1:3, ]
  after_ne <- rbind(after_ne, data.frame(x = NA_real_, y = 4L))
  v2 <- .hal_verify_transform(before, after_ne)
  expect_identical(v2$new_na_cols, "x")
  expect_identical(v2$na_increase_cols, character())
})

test_that("NA-increase on an already-NA column only fires when nrow unchanged", {
  before <- data.frame(x = c(NA, 1, 2))
  after <- data.frame(x = c(NA, NA, 2))
  v <- .hal_verify_transform(before, after)
  expect_identical(v$new_na_cols, character())   # not 0 -> >0
  expect_identical(v$na_increase_cols, "x")
  expect_match(.hal_verify_oneliner(v), "new NAs: x\\(1\\)")
})

test_that("identical output and zero rows are flagged", {
  v_id <- .hal_verify_transform(mtcars, mtcars)
  expect_true(v_id$identical_output)
  expect_identical(.hal_verify_oneliner(v_id),
                   "hal_do: output identical to input")

  v_zero <- .hal_verify_transform(mtcars, mtcars[0, ])
  expect_true(v_zero$zero_rows)
})

test_that("no structural changes yields the quiet oneliner", {
  before <- mtcars
  after <- mtcars
  after$mpg <- after$mpg * 2  # values change, structure doesn't
  v <- .hal_verify_transform(before, after)
  expect_false(v$identical_output)
  expect_identical(.hal_verify_oneliner(v), "hal_do: no structural changes")
})

test_that("format and print methods produce readable output", {
  v <- .hal_verify_transform(mtcars, mtcars[mtcars$mpg > 20, 1:5])
  lines <- format(v)
  expect_true(any(grepl("rows: 32 ->", lines)))
  expect_true(any(grepl("removed:", lines)))
  out <- capture.output(print(v))
  expect_match(out[1], "<hal_verification>")
})

test_that("tolerates list-columns and data.frame subclasses", {
  before <- data.frame(x = 1:3)
  before$lst <- list(1, NULL, 3)   # list-column; is.na(list(NULL)) is FALSE
  class(before) <- c("tbl_df", "tbl", "data.frame")
  after <- before[1:2, ]
  expect_no_error(v <- .hal_verify_transform(before, after))
  expect_identical(v$nrow_after, 2L)
})

test_that("tolerates duplicate column names", {
  before <- data.frame(x = 1:3, x = 4:6, check.names = FALSE)
  after <- before[1:2, ]
  expect_no_error(v <- .hal_verify_transform(before, after))
  expect_identical(v$nrow_after, 2L)
})

# ------------------------------------------------------------------------------
# .hal_do_verify -- the hal_do integration seam
# ------------------------------------------------------------------------------

test_that(".hal_do_verify attaches report and displays oneliner", {
  before <- mtcars
  after <- mtcars[mtcars$mpg > 20, ]
  msgs <- capture.output(
    out <- .hal_do_verify(before, after),
    type = "message"
  )
  expect_s3_class(attr(out, "hal_verify"), "hal_verification")
  expect_true(any(grepl("32 -> [0-9]+ rows", msgs)))
})

test_that(".hal_do_verify warns (classed) on identical output", {
  expect_warning(
    out <- .hal_do_verify(mtcars, mtcars),
    class = "hal_do_warning"
  )
  expect_true(attr(out, "hal_verify")$identical_output)
})

test_that(".hal_do_verify warns (classed) on zero-row output", {
  expect_warning(
    .hal_do_verify(mtcars, mtcars[0, ]),
    class = "hal_do_warning"
  )
})

test_that(".hal_do_verify is disabled by arg and by option", {
  suppressMessages({
    out1 <- .hal_do_verify(mtcars, mtcars, verify = FALSE)
    expect_null(attr(out1, "hal_verify"))

    withr::with_options(list(hal.verify = FALSE), {
      out2 <- .hal_do_verify(mtcars, mtcars)
      expect_null(attr(out2, "hal_verify"))
    })
  })
})

test_that(".hal_do_verify skips non-data-frame inputs silently", {
  expect_identical(.hal_do_verify(mtcars, "a string"), "a string")
  expect_identical(.hal_do_verify(1:3, mtcars), mtcars)
  expect_null(attr(.hal_do_verify(1:3, mtcars), "hal_verify"))
})
