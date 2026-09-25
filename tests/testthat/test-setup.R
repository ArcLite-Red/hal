# Tests for hal_setup (offline — tests logic without actually installing).
#
# These exercise the legacy Copilot-CLI setup path explicitly via
# `backend = "copilot"`, since the default may now resolve to vscode inside
# Positron (where developers commonly run tests).

test_that("hal_setup returns TRUE early when CLI already available", {
  skip_if_not(hal_available(backend = "copilot"), "Copilot CLI not found")

  expect_message(
    result <- hal_setup(force = FALSE, backend = "copilot"),
    "already installed"
  )
  expect_true(result)
})

test_that(".hal_cli_version returns a string or NULL", {
  version <- .hal_cli_version()
  # Either a version string or NULL (if CLI not installed)
  if (!is.null(version)) {
    expect_type(version, "character")
    expect_true(nzchar(version))
  }
})

test_that("hal_setup with force = TRUE attempts install even if available", {
  skip_if_not(hal_available(backend = "copilot"), "Copilot CLI not found")
  skip_if(Sys.which("npm") == "", "npm not available")

  expect_message(
    result <- hal_setup(force = TRUE, quiet = FALSE, backend = "copilot"),
    "install|found|setup",
    ignore.case = TRUE
  )
})

test_that(".hal_backend defaults to vscode in Positron, copilot elsewhere", {
  withr::with_envvar(c(POSITRON_VERSION = "test-1.0"), {
    withr::with_options(list(hal.backend = NULL), {
      expect_identical(hal:::.hal_backend(), "vscode")
    })
  })
  withr::with_envvar(c(POSITRON_VERSION = ""), {
    withr::with_options(list(hal.backend = NULL), {
      expect_identical(hal:::.hal_backend(), "copilot")
    })
  })
})

test_that("explicit hal.backend option overrides Positron detection", {
  withr::with_envvar(c(POSITRON_VERSION = "test-1.0"), {
    withr::with_options(list(hal.backend = "copilot"), {
      expect_identical(hal:::.hal_backend(), "copilot")
    })
  })
})


# -- Claude backend branch -----------------------------------------------------

test_that("hal_setup(backend = 'claude') routes to the Claude branch", {
  # Regression: hal_setup documented backend = "claude" but had no branch for
  # it, so it fell through and installed the *Copilot* CLI instead.
  routed <- NULL
  local_mocked_bindings(
    .hal_setup_claude = function(force, quiet) {
      routed <<- "claude"
      invisible(TRUE)
    }
  )
  hal_setup(backend = "claude", quiet = TRUE)
  expect_identical(routed, "claude")
})

test_that(".hal_setup_claude reports a missing CLI without installing anything", {
  local_mocked_bindings(
    hal_available = function(backend = NULL) FALSE,
    find_claude_cli = function(cli_path = NULL) stop("not found")
  )
  result <- suppressMessages(.hal_setup_claude(force = FALSE, quiet = TRUE))
  expect_false(result)
})

test_that(".hal_setup_claude short-circuits when the CLI already works", {
  local_mocked_bindings(
    hal_available = function(backend = NULL) TRUE,
    .hal_claude_cli_version = function() "1.2.3"
  )
  expect_message(
    result <- .hal_setup_claude(force = FALSE, quiet = TRUE),
    "already installed"
  )
  expect_true(result)
})

test_that(".hal_claude_cli_version returns a string or NULL", {
  version <- .hal_claude_cli_version()
  if (!is.null(version)) {
    expect_type(version, "character")
    expect_true(nzchar(version))
  }
})
