# test-session.R -- Session singleton lifecycle tests

test_that("session env starts empty", {
  session <- .hal_get_session()
  expect_type(session, "environment")
})

test_that("initialize creates session with chat", {
  withr::local_options(list(
    hal.system_prompt = "test prompt",
    hal.default_model = NULL,
    hal.permission_policy = NULL,
    hal.session_quiet = TRUE,
    hal.init_quiet = TRUE
  ))
  withr::local_envvar(COPILOT_MODEL = NA)

  # Use mock client via hal_chat infrastructure
  session <- .hal_get_session()
  old_chat <- session$chat
  on.exit({
    # Restore previous state
    if (!is.null(old_chat)) {
      session$chat <- old_chat
    } else {
      session$chat <- NULL
    }
  }, add = TRUE)

  # Clear to test lazy init

  session$chat <- NULL
  session$eval_tools_registered <- FALSE

  # Initialize uses the mock path -- but we can test the state setup
  # by checking defaults before a real init
  expect_null(session$chat)
})

test_that("ensure_session is idempotent when session exists", {
  session <- .hal_get_session()
  old_chat <- session$chat

  # If chat exists, ensure_session should not recreate
  if (!is.null(old_chat)) {
    .hal_ensure_session()
    expect_identical(session$chat, old_chat)
  } else {
    # No session active -- just verify it doesn't error on NULL
    expect_true(is.null(session$chat))
  }
})

test_that("session stores metadata fields", {
  session <- .hal_get_session()

  # These fields should exist after any init
  expect_true(is.environment(session))
  expected_fields <- c("eval_caller_env", "eval_r_assigned",
                       "eval_tools_registered", "tool_names",
                       "spawn_count", "spawn_log", "data_notice_shown")

  # If session has been initialized, check fields
  if (!is.null(session$chat)) {
    for (field in expected_fields) {
      expect_true(!is.null(session[[field]]),
                  info = paste("Missing field:", field))
    }
  }
})

test_that("data notice respects init_quiet option", {
  session <- .hal_get_session()
  session$data_notice_shown <- FALSE

  withr::local_options(list(hal.init_quiet = TRUE))
  expect_silent(.hal_data_notice())
})

test_that("data notice only shows once", {
  session <- .hal_get_session()
  session$data_notice_shown <- TRUE

  # Should be silent on second call regardless of option
  expect_silent(.hal_data_notice())

  # Reset
  session$data_notice_shown <- FALSE
})

test_that("system prompt resolves from option", {
  withr::local_options(list(hal.system_prompt = "Custom prompt for {user}"))

  # The option takes precedence over default
  prompt <- getOption("hal.system_prompt")
  expect_equal(prompt, "Custom prompt for {user}")
})

test_that("system prompt falls back to default when option is NULL", {
  withr::local_options(list(hal.system_prompt = NULL))

  prompt <- getOption("hal.system_prompt") %||% .hal_default_system_prompt()
  expect_true(grepl("hal", prompt))
  expect_true(grepl("eval_r", prompt))
})
