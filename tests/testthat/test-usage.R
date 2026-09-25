# Tests for hal_usage() — session cost visibility

# ---- tier numeric conversion ------------------------------------------------

test_that(".hal_tier_numeric parses canonical tiers", {
  expect_equal(.hal_tier_numeric("1x"), 1)
  expect_equal(.hal_tier_numeric("0.33x"), 0.33)
  expect_equal(.hal_tier_numeric("3x"), 3)
  expect_equal(.hal_tier_numeric("0x"), 0)
})

test_that(".hal_tier_numeric returns NA for unknown or empty", {
  expect_true(is.na(.hal_tier_numeric(NA_character_)))
  expect_true(is.na(.hal_tier_numeric("")))
  expect_true(is.na(.hal_tier_numeric("weird")))
})

test_that(".hal_tier_numeric vectorises", {
  out <- .hal_tier_numeric(c("1x", "3x", NA, "0.33x"))
  expect_equal(out, c(1, 3, NA, 0.33))
})

# ---- .hal_record_turn -------------------------------------------------------

test_that(".hal_record_turn appends entries to session$usage_log", {
  session <- .hal_get_session()
  old_log <- session$usage_log
  old_model <- session$model
  on.exit({
    session$usage_log <- old_log
    session$model <- old_model
  }, add = TRUE)

  session$usage_log <- list()
  session$model <- "gpt-4.1"

  .hal_record_turn()
  .hal_record_turn(model = "claude-sonnet-4.6")

  expect_length(session$usage_log, 2L)
  expect_equal(session$usage_log[[1]]$model, "gpt-4.1")
  expect_equal(session$usage_log[[2]]$model, "claude-sonnet-4.6")
  expect_s3_class(session$usage_log[[1]]$timestamp, "POSIXct")
})

# ---- hal_usage() aggregation ------------------------------------------------

test_that("hal_usage returns empty data frame when no turns logged", {
  session <- .hal_get_session()
  old_log <- session$usage_log
  on.exit(session$usage_log <- old_log, add = TRUE)

  session$usage_log <- list()
  u <- hal_usage()

  expect_s3_class(u, "hal_usage")
  expect_s3_class(u, "data.frame")
  expect_equal(nrow(u), 0L)
  expect_equal(attr(u, "total_turns"), 0L)
  expect_equal(attr(u, "total_units"), 0)
})

test_that("hal_usage tallies turns per model with supplied tiers", {
  session <- .hal_get_session()
  old_log <- session$usage_log
  old_tiers <- session$model_tiers
  on.exit({
    session$usage_log <- old_log
    session$model_tiers <- old_tiers
  }, add = TRUE)

  session$usage_log <- list(
    list(model = "gpt-4.1", timestamp = Sys.time()),
    list(model = "gpt-4.1", timestamp = Sys.time()),
    list(model = "claude-opus-4.6", timestamp = Sys.time())
  )
  session$model_tiers <- NULL  # force use of supplied tiers

  tiers <- c("gpt-4.1" = "1x", "claude-opus-4.6" = "3x")
  u <- hal_usage(tiers = tiers)

  # Sorted by turns desc
  expect_equal(u$model, c("gpt-4.1", "claude-opus-4.6"))
  expect_equal(u$turns, c(2L, 1L))
  expect_equal(u$tier, c("1x", "3x"))
  expect_equal(u$units, c(2, 3))
  expect_equal(attr(u, "total_turns"), 3L)
  expect_equal(attr(u, "total_units"), 5)
})

test_that("hal_usage handles unknown tiers as NA units", {
  session <- .hal_get_session()
  old_log <- session$usage_log
  old_tiers <- session$model_tiers
  on.exit({
    session$usage_log <- old_log
    session$model_tiers <- old_tiers
  }, add = TRUE)

  session$usage_log <- list(
    list(model = "mystery-model", timestamp = Sys.time())
  )
  session$model_tiers <- NULL

  u <- hal_usage(tiers = character())

  expect_equal(u$model, "mystery-model")
  expect_equal(u$turns, 1L)
  expect_true(is.na(u$tier))
  expect_true(is.na(u$units))
})

test_that("hal_usage labels NULL model as '(default)'", {
  session <- .hal_get_session()
  old_log <- session$usage_log
  old_tiers <- session$model_tiers
  on.exit({
    session$usage_log <- old_log
    session$model_tiers <- old_tiers
  }, add = TRUE)

  session$usage_log <- list(list(model = NULL, timestamp = Sys.time()))
  session$model_tiers <- NULL

  u <- hal_usage(tiers = character())
  expect_equal(u$model, "(default)")
})

test_that("hal_usage caches supplied tiers on the session", {
  session <- .hal_get_session()
  old_log <- session$usage_log
  old_tiers <- session$model_tiers
  on.exit({
    session$usage_log <- old_log
    session$model_tiers <- old_tiers
  }, add = TRUE)

  session$usage_log <- list(list(model = "gpt-4.1", timestamp = Sys.time()))
  session$model_tiers <- NULL

  tiers <- c("gpt-4.1" = "1x")
  hal_usage(tiers = tiers)
  expect_equal(session$model_tiers, tiers)
})

# ---- print method -----------------------------------------------------------

test_that("print.hal_usage works for empty and populated tables", {
  session <- .hal_get_session()
  old_log <- session$usage_log
  on.exit(session$usage_log <- old_log, add = TRUE)

  # Empty — cli_alert_info routes through message()
  session$usage_log <- list()
  empty <- hal_usage()
  msgs <- capture.output(
    print(empty),
    type = "message"
  )
  expect_true(any(grepl("No turns recorded", msgs)))

  # Populated
  session$usage_log <- list(
    list(model = "gpt-4.1", timestamp = Sys.time()),
    list(model = "gpt-4.1", timestamp = Sys.time())
  )
  u <- hal_usage(tiers = c("gpt-4.1" = "1x"))
  out <- capture.output(print(u))
  expect_true(any(grepl("gpt-4.1", out)))
  expect_true(any(grepl("Total: 2 turns", out)))
})
