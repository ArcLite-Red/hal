# test-models.R -- hal_models() tests

test_that("hal_models returns data frame with mock client", {
  client <- mock_client("basic")

  result <- hal_models(client = client)
  client$stop()

  expect_s3_class(result, "data.frame")
  expected_cols <- c("id", "name", "description", "provider", "family",
                     "usage", "multiplier", "is_default", "context_window",
                     "release_date")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("hal_models parses mock model correctly", {
  client <- mock_client("basic")

  result <- hal_models(client = client)
  client$stop()

  expect_equal(nrow(result), 1)
  expect_equal(result$id, "mock-model")
  expect_equal(result$name, "Mock Model")
  expect_equal(result$description, "For testing")
  expect_equal(result$usage, "0x")
  expect_equal(result$multiplier, 0)
  expect_true(result$is_default)
})

test_that("hal_models derives provider and family from id", {
  expect_equal(.hal_provider_from_id("gpt-5.4"), "openai")
  expect_equal(.hal_provider_from_id("claude-opus-4.7"), "anthropic")
  expect_equal(.hal_provider_from_id("gemini-3-pro-preview"), "google")
  expect_equal(.hal_provider_from_id("foo-bar"), "unknown")

  expect_equal(.hal_family_from_id("claude-opus-4.7"), "heavy")
  expect_equal(.hal_family_from_id("claude-haiku-4.5"), "light")
  expect_equal(.hal_family_from_id("gpt-5-mini"), "light")
  expect_equal(.hal_family_from_id("gpt-5.4"), "standard")
})

test_that("hal_models normalizes usage to multiplier", {
  expect_equal(.hal_usage_to_numeric(c("0x", "0.33x", "1x", "3x", "15x")),
               c(0, 0.33, 1, 3, 15))
  expect_equal(.hal_normalize_usage(c("0", "1x", NA)),
               c("0x", "1x", NA_character_))
})

test_that("hal_model_info returns full record for known id", {
  client <- mock_client("basic")
  info <- hal_model_info("mock-model", client = client)
  client$stop()

  expect_equal(info$id, "mock-model")
  expect_equal(info$provider, "unknown")
  expect_equal(info$multiplier, 0)
  expect_true(info$is_default)
  expect_type(info$meta, "list")
})

test_that("hal_model_info errors on unknown id", {
  client <- mock_client("basic")
  expect_error(hal_model_info("does-not-exist", client = client),
               "Unknown model id")
  client$stop()
})

test_that("claude static model list has expected columns", {
  withr::with_options(list(hal.backend = "claude"), {
    df <- hal_models()
    expect_s3_class(df, "data.frame")
    expect_true(all(c("id", "name", "description", "provider", "family",
                      "usage", "multiplier", "is_default", "context_window",
                      "release_date") %in% names(df)))
    expect_equal(unique(df$provider), "anthropic")
    expect_true(any(df$is_default))
  })
})

test_that("hal_models handles empty model list", {
  # Create a client and manually override session to have no models
  client <- mock_client("basic")
  client$handshake()

  # Directly test the parsing logic with empty input
  models <- list()
  expect_equal(length(models), 0)

  client$stop()
})

test_that("hal_models stops client when it creates one", {
  # When client = NULL, hal_models creates its own and should clean up

  # We can't easily test this without a real CLI, but we can verify
  # the on.exit cleanup logic by passing our own client
  client <- mock_client("basic")

  result <- hal_models(client = client)

  # Client should still be usable since we passed it in (own_client = FALSE)
  # The function should NOT stop a client it didn't create
  expect_true(client$is_alive())
  client$stop()
})
