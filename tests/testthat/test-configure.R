# Tests for hal configure module

test_that("hal_configure sets options", {
  withr::with_options(list(
    hal.use_colors = NULL,
    hal.stream = NULL,
    hal.eval_timeout = NULL
  ), {
    hal_configure(
      use_colors = FALSE,
      stream = TRUE,
      eval_timeout = 60,
      quiet = TRUE
    )
    expect_false(getOption("hal.use_colors"))
    expect_true(getOption("hal.stream"))
    expect_equal(getOption("hal.eval_timeout"), 60)
  })
})

test_that("hal_configure validates stream_speed", {
  expect_error(
    hal_configure(stream_speed = "turbo", quiet = TRUE),
    "stream_speed"
  )
})

test_that("hal_configure validates credential_action", {
  expect_error(
    hal_configure(credential_action = "delete", quiet = TRUE),
    "credential_action"
  )
})

test_that("hal_config returns all settings", {
  cfg <- hal_config()
  expect_type(cfg, "list")
  expect_true("use_colors" %in% names(cfg))
  expect_true("credential_action" %in% names(cfg))
  expect_true("eval_timeout" %in% names(cfg))
  expect_true("current_user" %in% names(cfg))
})

test_that("hal_configure substitutes {user} in system_prompt", {
  withr::with_options(list(hal.system_prompt = NULL), {
    hal_configure(system_prompt = "Hello {user}", quiet = TRUE)
    prompt <- getOption("hal.system_prompt")
    expect_false(grepl("\\{user\\}", prompt))
    expect_true(nzchar(prompt))
  })
})

test_that("hal_configure sets do_retries", {
  withr::with_options(list(hal.do_retries = NULL), {
    hal_configure(do_retries = 3, quiet = TRUE)
    expect_equal(getOption("hal.do_retries"), 3L)
  })
})

test_that("hal_configure validates do_retries", {
  expect_error(
    hal_configure(do_retries = -1, quiet = TRUE),
    "do_retries"
  )
})

test_that("hal_config includes do_retries", {
  cfg <- hal_config()
  expect_true("do_retries" %in% names(cfg))
})

test_that("hal_configure sets permission_policy string", {
  withr::with_options(list(hal.permission_policy = NULL), {
    hal_configure(permission_policy = "auto-deny", quiet = TRUE)
    expect_equal(getOption("hal.permission_policy"), "auto-deny")
  })
})

test_that("hal_configure sets permission_policy function", {
  withr::with_options(list(hal.permission_policy = NULL), {
    custom_fn <- function(params) "allow_once"
    hal_configure(permission_policy = custom_fn, quiet = TRUE)
    expect_true(is.function(getOption("hal.permission_policy")))
  })
})

test_that("hal_configure validates permission_policy", {
  expect_error(
    hal_configure(permission_policy = "yolo", quiet = TRUE),
    "permission_policy"
  )
})

test_that("hal_configure sets session_quiet", {
  withr::with_options(list(hal.session_quiet = NULL), {
    hal_configure(session_quiet = TRUE, quiet = TRUE)
    expect_true(getOption("hal.session_quiet"))
  })
})

test_that("hal_config includes permission_policy and session_quiet", {
  cfg <- hal_config()
  expect_true("permission_policy" %in% names(cfg))
  expect_true("session_quiet" %in% names(cfg))
})
