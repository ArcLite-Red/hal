# hal_status() -- consolidated diagnostic

test_that("hal_status returns a structured list with the expected fields", {
  withr::with_options(list(hal.backend = "copilot"), {
    withr::with_envvar(c(COPILOT_CLI_PATH = ""), {
      withr::with_path("", action = "replace", {
        res <- suppressMessages(hal_status())
        expect_type(res, "list")
        expect_named(
          res,
          c("backend", "backend_source", "available", "detail",
            "session_active", "model", "next_step")
        )
        expect_identical(res$backend, "copilot")
        expect_false(res$available)
        expect_type(res$next_step, "character")
      })
    })
  })
})

test_that("hal_status reports backend source as option when set", {
  withr::with_options(list(hal.backend = "claude"), {
    withr::with_envvar(c(CLAUDE_CLI_PATH = ""), {
      withr::with_path("", action = "replace", {
        res <- suppressMessages(hal_status())
        expect_identical(res$backend, "claude")
        expect_identical(res$backend_source, "hal.backend option")
      })
    })
  })
})

test_that("hal_status vscode probe fails cleanly outside Positron", {
  withr::with_options(list(hal.backend = "vscode"), {
    withr::with_envvar(c(POSITRON_VERSION = ""), {
      res <- suppressMessages(hal_status())
      expect_false(res$available)
      expect_match(res$detail, "not inside Positron")
      expect_match(res$next_step, "hal_configure")
    })
  })
})

test_that("hal_status surfaces an invalid backend option without erroring", {
  withr::with_options(list(hal.backend = "nonsense"), {
    res <- suppressMessages(hal_status())
    expect_true(is.na(res$backend))
    expect_false(res$available)
    expect_match(res$next_step, "hal_configure")
  })
})

# ---- missing Copilot, Claude available -------------------------------------

test_that("status names the Claude switch when Copilot is missing but Claude is installed", {
  local_mocked_bindings(
    find_hal_cli = function(...) stop("not found"),
    .hal_claude_installed = function() TRUE
  )
  res <- .hal_status_cli("copilot")
  expect_false(res$ok)
  expect_match(res$detail, "Claude Code is installed")
  expect_match(res$next_step, 'hal_configure(backend = "claude")', fixed = TRUE)
})

test_that("status keeps the setup step when Copilot is missing and Claude is too", {
  local_mocked_bindings(
    find_hal_cli = function(...) stop("not found"),
    .hal_claude_installed = function() FALSE
  )
  res <- .hal_status_cli("copilot")
  expect_false(res$ok)
  expect_match(res$next_step, "hal_setup()", fixed = TRUE)
  expect_no_match(res$next_step, "claude")
})
