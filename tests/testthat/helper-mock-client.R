# Test helper: create HalClient/HalChat backed by mock CLI
#
# The mock CLI script (inst/mock-cli/mock_copilot.R) speaks the same NDJSON
# protocol as the real Copilot CLI. Scenarios control prompt response behavior.

mock_client <- function(scenario = "basic", model = NULL,
                        permission_policy = "auto-allow",
                        on_text = NULL, on_tool_call = NULL,
                        on_thought = NULL) {
  # Mock-CLI tests spawn Rscript subprocesses via processx -- reliable on
  # CI we control, but skipped on CRAN check farms per policy prudence.
  testthat::skip_on_cran()
  rscript <- normalizePath(
    file.path(R.home("bin"),
              if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    mustWork = TRUE
  )

  mock_script <- system.file("mock-cli", "mock_copilot.R", package = "hal")
  if (!nzchar(mock_script)) {
    testthat::skip("Mock CLI script not found (package not installed?)")
  }

  client <- HalClient$new(
    cli_path = rscript,
    model = model,
    permission_policy = permission_policy,
    on_text = on_text,
    on_tool_call = on_tool_call,
    on_thought = on_thought,
    quiet = TRUE
  )

  # Override cli_info so start() runs: Rscript --vanilla mock_copilot.R <scenario> --acp
  client$.__enclos_env__$private$cli_info <- list(
    command = rscript,
    prefix_args = c("--vanilla", mock_script, scenario)
  )

  client
}

mock_chat <- function(scenario = "basic", ...) {
  client <- mock_client(scenario, ...)
  HalChat$new(client = client, echo = "none", quiet = TRUE)
}

# -- Claude backend mock -----------------------------------------------------
# The Claude mock lives at inst/mock-cli/mock_claude.R and emits stream-json
# NDJSON identical to what `claude -p --output-format stream-json` produces.
# Unlike Copilot (one long-running ACP server), Claude spawns a fresh process
# per turn, so every $prompt() call re-invokes Rscript + the mock script.
#
# The mock's multi-turn scenario writes state files across subprocess
# invocations; route them into a per-run tempdir via HAL_MOCK_STATE_DIR so
# R CMD check doesn't flag stray rds files in the temp root. Env vars set
# here are inherited by processx subprocesses.
# tempfile() returns a unique path under tempdir(); the mock's dir.create()
# materialises it on first write, and tempdir() is cleaned by R on exit.
Sys.setenv(HAL_MOCK_STATE_DIR = tempfile("hal_mock_claude_"))

mock_claude_client <- function(scenario = "basic",
                               model = "mock-claude-haiku",
                               permission_policy = "auto-allow",
                               on_text = NULL, on_tool_call = NULL,
                               on_thought = NULL) {
  testthat::skip_on_cran()
  rscript <- normalizePath(
    file.path(R.home("bin"),
              if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    mustWork = TRUE
  )

  mock_script <- system.file("mock-cli", "mock_claude.R", package = "hal")
  if (!nzchar(mock_script)) {
    testthat::skip("Mock Claude CLI script not found (package not installed?)")
  }

  # Construct bypassing find_claude_cli — the constructor validates the path.
  # We supply rscript as cli_path; the override below replaces the command +
  # args that $prompt() actually invokes.
  client <- HalClientClaude$new(
    model = model,
    cli_path = rscript,
    permission_policy = permission_policy,
    on_text = on_text,
    on_tool_call = on_tool_call,
    on_thought = on_thought,
    quiet = TRUE
  )

  # Override cli_info so $prompt() runs:
  #   Rscript --vanilla mock_claude.R <scenario> <real claude flags...>
  # The client prepends cli_info$prefix_args ahead of the built arg list.
  client$.__enclos_env__$private$cli_info <- list(
    command = rscript,
    prefix_args = c("--vanilla", mock_script, scenario)
  )

  client
}

mock_claude_chat <- function(scenario = "basic", ...) {
  client <- mock_claude_client(scenario, ...)
  HalChat$new(client = client, echo = "none", quiet = TRUE)
}
