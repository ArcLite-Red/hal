# Tests for hal governance module

test_that("denylist blocks system() calls", {
  msg <- .hal_check_eval_denylist("system('ls')")
  expect_match(msg, "system")
  expect_match(msg, "Blocked")
})

test_that("denylist blocks namespaced calls", {
  msg <- .hal_check_eval_denylist("base::system('ls')")
  expect_match(msg, "system")
})

test_that("denylist passes clean code", {
  msg <- .hal_check_eval_denylist("1 + 1")
  expect_null(msg)
})

test_that("denylist passes when disabled via FALSE", {
  withr::with_options(list(hal.eval_denylist = FALSE), {
    msg <- .hal_check_eval_denylist("system('ls')")
    expect_null(msg)
  })
})

test_that("denylist handles parse errors gracefully", {
  msg <- .hal_check_eval_denylist("if (")
  expect_null(msg)  # let eval report the error
})

test_that("denylist blocks file writers (crossover with SDK write tools)", {
  for (fn in c("writeLines", "write.csv", "write.table", "writeBin",
               "saveRDS", "save", "save.image", "sink",
               "file.create", "file.copy", "dir.create")) {
    code <- paste0(fn, "(x)")
    msg <- .hal_check_eval_denylist(code)
    expect_match(msg, fn, fixed = TRUE,
                 info = paste("expected denylist to block", fn))
  }
})

test_that("denylist blocks env/state mutation functions", {
  for (fn in c("rm", "assign", "setwd")) {
    code <- paste0(fn, "(x)")
    msg <- .hal_check_eval_denylist(code)
    expect_match(msg, fn, fixed = TRUE,
                 info = paste("expected denylist to block", fn))
  }
})

test_that("denylist blocks package surgery functions", {
  for (fn in c("install.packages", "remove.packages", "update.packages")) {
    code <- paste0(fn, "('pkg')")
    msg <- .hal_check_eval_denylist(code)
    expect_match(msg, fn, fixed = TRUE,
                 info = paste("expected denylist to block", fn))
  }
})

test_that("denylist passes R env manipulation code", {
  # eval_r's real purpose -- these must not be blocked
  expect_null(.hal_check_eval_denylist("df <- mtcars"))
  expect_null(.hal_check_eval_denylist("mean(df$mpg)"))
  expect_null(.hal_check_eval_denylist("df$new_col <- df$mpg * 2"))
  expect_null(.hal_check_eval_denylist("summary(lm(mpg ~ wt, data = df))"))
  expect_null(.hal_check_eval_denylist("options(scipen = 999)"))
  expect_null(.hal_check_eval_denylist("library(dplyr)"))
})

test_that("credential scanner detects GitHub PAT", {
  result <- .hal_scan_credentials("token: ghp_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789")
  expect_true(result$found)
  expect_true("github_pat" %in% result$matches)
})

test_that("credential scanner cleans detected patterns", {
  result <- .hal_scan_credentials("key: ghp_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789")
  expect_match(result$cleaned, "REDACTED", fixed = TRUE)
})

test_that("credential scanner passes clean text", {
  result <- .hal_scan_credentials("Hello, this is a normal message")
  expect_false(result$found)
})

test_that("scan_outbound respects warn action", {
  withr::with_options(list(hal.credential_action = "warn"), {
    expect_warning(
      .hal_scan_outbound("ghp_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789"),
      "credentials"
    )
  })
})

test_that("scan_outbound respects redact action", {
  withr::with_options(list(hal.credential_action = "redact"), {
    result <- suppressWarnings(
      .hal_scan_outbound("ghp_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789")
    )
    expect_match(result, "REDACTED", fixed = TRUE)
  })
})

test_that("scan_outbound respects block action", {
  withr::with_options(list(hal.credential_action = "block"), {
    expect_error(
      .hal_scan_outbound("ghp_aBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789"),
      "blocked"
    )
  })
})

test_that("eval_with_timeout works for fast code", {
  result <- .hal_eval_with_timeout(quote(1 + 1), globalenv())
  expect_equal(result, 2)
})


