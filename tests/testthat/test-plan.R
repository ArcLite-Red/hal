# Tests for hal_plan() — SDK plan.md path surfacing

# ---- path helper ------------------------------------------------------------

test_that(".hal_plan_source_path resolves under the configured root", {
  withr::with_options(list(hal.plan_root = "/tmp/fake-root"), {
    p <- .hal_plan_source_path("abc-123")
    expect_equal(
      p,
      file.path(path.expand("/tmp/fake-root"), "abc-123", "plan.md")
    )
  })
})

test_that(".hal_plan_source_path defaults to ~/.copilot/session-state", {
  withr::with_options(list(hal.plan_root = NULL), {
    p <- .hal_plan_source_path("abc-123")
    expected <- file.path(
      path.expand("~/.copilot/session-state"), "abc-123", "plan.md"
    )
    expect_equal(p, expected)
  })
})

# ---- hal_plan() public API --------------------------------------------------

test_that("hal_plan returns NULL with info when no session", {
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- NULL
  on.exit(session$chat <- old_chat, add = TRUE)

  expect_message(result <- hal_plan(), "No active hal session")
  expect_null(result)
})

test_that("hal_plan returns NULL when session id not yet available", {
  session <- .hal_get_session()
  old_chat <- session$chat
  fake_client <- list(get_session_id = function() NULL)
  session$chat <- list(get_client = function() fake_client)
  on.exit(session$chat <- old_chat, add = TRUE)

  expect_message(result <- hal_plan(), "not yet been created")
  expect_null(result)
})

test_that("hal_plan returns NULL when plan.md doesn't exist yet", {
  tmp <- withr::local_tempdir()
  sid <- "sid-no-plan"

  session <- .hal_get_session()
  old_chat <- session$chat
  fake_client <- list(get_session_id = function() sid)
  session$chat <- list(get_client = function() fake_client)
  on.exit(session$chat <- old_chat, add = TRUE)

  withr::with_options(list(hal.plan_root = tmp), {
    expect_message(result <- hal_plan(), "No.*plan\\.md")
    expect_null(result)
  })
})

test_that("hal_plan returns path when plan.md exists", {
  tmp <- withr::local_tempdir()
  sid <- "sid-with-plan"
  plan_dir <- file.path(tmp, sid)
  dir.create(plan_dir, recursive = TRUE)
  plan_file <- file.path(plan_dir, "plan.md")
  writeLines(c("# Plan", "Step 1: do a thing"), plan_file)

  session <- .hal_get_session()
  old_chat <- session$chat
  fake_client <- list(get_session_id = function() sid)
  session$chat <- list(get_client = function() fake_client)
  on.exit(session$chat <- old_chat, add = TRUE)

  withr::with_options(list(hal.plan_root = tmp), {
    result <- hal_plan()
    expect_equal(result, plan_file)
  })
})
