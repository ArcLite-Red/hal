test_that("hal_available(backend = 'copilot') is FALSE when CLI not installed", {
  withr::with_envvar(c(COPILOT_CLI_PATH = ""), {
    withr::with_path("", action = "replace", {
      expect_false(hal_available(backend = "copilot"))
    })
  })
})

# ---- pointing a Copilot-less user at Claude --------------------------------

test_that(".hal_claude_hint is empty when Claude Code is not installed", {
  local_mocked_bindings(.hal_claude_installed = function() FALSE)
  expect_length(.hal_claude_hint(), 0L)
})

test_that(".hal_claude_hint names the backend switch when Claude is installed", {
  local_mocked_bindings(.hal_claude_installed = function() TRUE)
  hint <- .hal_claude_hint()
  expect_named(hint, "i")
  expect_match(hint, 'backend = \\"claude\\"', fixed = FALSE)
})

test_that("missing-Copilot error leads with Claude when it is installed", {
  local_mocked_bindings(.hal_claude_installed = function() TRUE)
  # Render through cli for real: quotes inside {.code} fail at render time,
  # not at parse time, so check the message actually builds.
  err <- tryCatch(cli::cli_abort(.hal_copilot_missing_msg()), error = identity)
  msg <- conditionMessage(err)
  expect_match(msg, "Could not find the Copilot CLI")
  expect_match(msg, "Claude Code is already installed")
  expect_match(msg, 'hal_configure(backend = "claude")', fixed = TRUE)
  # Install instructions stay -- the hint is an alternative, not a replacement.
  expect_match(msg, "hal_setup()", fixed = TRUE)
})

test_that("missing-Copilot error omits the Claude hint when Claude is absent", {
  local_mocked_bindings(.hal_claude_installed = function() FALSE)
  msg <- conditionMessage(
    tryCatch(cli::cli_abort(.hal_copilot_missing_msg()), error = identity)
  )
  expect_match(msg, "Could not find the Copilot CLI")
  expect_no_match(msg, "Claude")
})
