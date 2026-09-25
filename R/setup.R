# ==============================================================================
# hal_setup -- Guided CLI installation and verification
# ==============================================================================

#' Set up hal for your environment
#'
#' Interactive helper that picks the right backend for the host and walks
#' through prerequisites. Two paths:
#'
#' - **Positron** (recommended): installs the hal-bridge extension from the
#'   VSIX bundled with hal, so hal talks to `vscode.lm` directly. No
#'   Node.js, no Copilot CLI, no GitHub CLI, no download -- the only
#'   requirement is that you are signed in to GitHub Copilot inside
#'   Positron itself (the account menu in the lower left).
#' - **Other hosts** (RStudio, VS Code, command-line R): installs the
#'   GitHub Copilot CLI (`gh copilot` extension preferred, npm
#'   `@github/copilot` fallback).
#'
#' - **Claude Code**: verifies the `claude` CLI and points at the download
#'   and one-time sign-in. hal does not install this one for you: Claude
#'   Code's sign-in is an interactive browser flow, and installing it via
#'   npm yields a shim whose output is unreliable when driven from R.
#'
#' Detection is via `POSITRON_VERSION`. Pass `backend = "copilot"` to force
#' the Copilot CLI path inside Positron, or `backend = "claude"` to check the
#' Claude Code CLI instead.
#'
#' @param force Logical; re-run setup even if the backend is already ready.
#' @param quiet Logical; suppress progress messages.
#' @param backend Optional backend override (`"vscode"`, `"copilot"`,
#'   `"claude"`). When `NULL`, picks `vscode` in Positron and `copilot`
#'   elsewhere.
#'
#' @return Invisibly `TRUE` if the backend transport is available after
#'   setup, `FALSE` otherwise.
#'
#' @seealso [hal_available()], [hal_install_bridge()],
#'   [hal_bridge_status()], [hal_models()]
#'
#' @export
#' @examples
#' \dontrun{
#' hal_setup()                          # auto: vscode in Positron, copilot elsewhere
#' hal_setup(backend = "copilot")       # force the Copilot CLI path
#' hal_setup(backend = "claude")        # check the Claude Code CLI
#' }
hal_setup <- function(force = FALSE, quiet = FALSE, backend = NULL) {

  # Resolve which backend we're setting up. .hal_backend() picks vscode in
  # Positron by default, copilot elsewhere; honors an explicit override.
  b <- .hal_backend(backend)

  if (identical(b, "vscode")) {
    return(.hal_setup_positron(force = force, quiet = quiet))
  }

  if (identical(b, "claude")) {
    return(.hal_setup_claude(force = force, quiet = quiet))
  }

  # Step 0: Already installed?
  if (!force && hal_available(backend = b)) {
    cli::cli_alert_success("Copilot CLI is already installed and working.")
    version <- .hal_cli_version()
    if (!is.null(version)) {
      cli::cli_alert_info("Version: {version}")
    }
    cli::cli_alert_info("Run {.code hal_setup(force = TRUE)} to reinstall.")
    return(invisible(TRUE))
  }

  cli::cli_h1("hal setup")

  # ---- Path A: GitHub CLI (gh copilot) -- preferred, no Node.js needed ----

  gh_path <- Sys.which("gh")

  if (nzchar(gh_path)) {
    if (!quiet) cli::cli_alert_success("GitHub CLI found: {.path {gh_path}}")

    # Check if copilot extension is installed
    has_copilot <- .gh_has_copilot(gh_path)

    if (!has_copilot) {
      if (!quiet) cli::cli_alert_info("Installing the {.pkg gh-copilot} extension...")
      ext_result <- tryCatch(
        processx::run(
          as.character(gh_path),
          c("extension", "install", "github/gh-copilot"),
          error_on_status = FALSE,
          timeout = 120
        ),
        error = function(e) list(status = 1L, stderr = e$message)
      )
      if (ext_result$status != 0L) {
        stderr <- trimws(ext_result$stderr %||% "")
        if (!quiet) {
          cli::cli_alert_warning("Extension install failed.")
          if (nzchar(stderr)) cli::cli_alert_warning("Output: {stderr}")
          cli::cli_alert_info("Try manually: {.code gh extension install github/gh-copilot}")
        }
      } else {
        has_copilot <- .gh_has_copilot(gh_path)
      }
    }

    if (has_copilot) {
      if (!quiet) cli::cli_alert_success("gh copilot is available.")
      return(.hal_setup_finish(quiet))
    }
  }

  # ---- Path B: Standalone npm install -- fallback, requires Node.js ----

  npm <- Sys.which("npm")
  node <- Sys.which("node")

  if (nzchar(npm) && nzchar(node)) {
    node_version <- tryCatch(
      trimws(processx::run("node", "--version", error_on_status = FALSE)$stdout),
      error = function(e) "unknown"
    )
    if (!quiet) cli::cli_alert_success("Node.js found: {node_version}")
    if (!quiet) cli::cli_alert_info("Installing the Copilot CLI via npm...")

    install_result <- tryCatch(
      processx::run(
        as.character(npm),
        c("install", "-g", "@github/copilot"),
        error_on_status = FALSE,
        timeout = 120
      ),
      error = function(e) list(status = 1L, stderr = e$message)
    )

    if (install_result$status == 0L) {
      if (!quiet) cli::cli_alert_success("Copilot CLI installed via npm.")

      if (!hal_available()) {
        cli::cli_alert_warning(
          "CLI installed but not found on PATH. You may need to restart your IDE."
        )
        npm_bin <- tryCatch(
          trimws(processx::run(as.character(npm), c("bin", "-g"),
                               error_on_status = FALSE)$stdout),
          error = function(e) ""
        )
        if (nzchar(npm_bin)) {
          cli::cli_alert_info("npm global bin: {.path {npm_bin}}")
          cli::cli_alert_info("Make sure this directory is on your PATH.")
        }
        return(invisible(FALSE))
      }

      return(.hal_setup_finish(quiet))
    }

    # npm failed
    stderr <- trimws(install_result$stderr %||% "")
    if (!quiet) {
      cli::cli_alert_warning("npm install failed.")
      if (nzchar(stderr)) cli::cli_alert_warning("npm output: {stderr}")
    }
  }

  # ---- Neither path available ----

  cli::cli_alert_danger("No installation method available.")
  claude_hint <- .hal_claude_hint()
  if (length(claude_hint)) cli::cli_bullets(claude_hint)
  cli::cli_alert_info("Install one of the following, then run {.code hal_setup()} again:")
  if (.Platform$OS.type == "windows") {
    cli::cli_bullets(c(
      "i" = "GitHub CLI (recommended, no Node.js needed):",
      " " = "  {.code winget install GitHub.cli}",
      " " = "  then: {.code gh extension install github/gh-copilot}",
      "i" = "Or Node.js (for standalone Copilot CLI):",
      " " = "  {.code winget install OpenJS.NodeJS}"
    ))
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    cli::cli_bullets(c(
      "i" = "GitHub CLI (recommended, no Node.js needed):",
      " " = "  {.code brew install gh}",
      " " = "  then: {.code gh extension install github/gh-copilot}",
      "i" = "Or Node.js (for standalone Copilot CLI):",
      " " = "  {.code brew install node}"
    ))
  } else {
    cli::cli_bullets(c(
      "i" = "GitHub CLI (recommended, no Node.js needed):",
      " " = "  {.url https://cli.github.com}",
      " " = "  then: {.code gh extension install github/gh-copilot}",
      "i" = "Or Node.js (for standalone Copilot CLI):",
      " " = "  {.code sudo apt install nodejs npm}  (Debian/Ubuntu)",
      " " = "  {.code sudo dnf install nodejs npm}  (Fedora)"
    ))
  }
  cli::cli_alert_warning("Restart your IDE (Positron/RStudio) after installing.")
  return(invisible(FALSE))
}


# -- Setup helpers -------------------------------------------------------------

#' Check if gh has the copilot extension
#' @param gh_path Path to the gh binary.
#' @return Logical.
#' @noRd
.gh_has_copilot <- function(gh_path) {
  check <- tryCatch(
    processx::run(as.character(gh_path), c("copilot", "--", "--version"),
                  timeout = 10, error_on_status = FALSE),
    error = function(e) list(status = 1L)
  )
  check$status == 0L
}

#' Finish setup: verify, show version, run auth
#' @param quiet Suppress messages.
#' @return Invisibly TRUE/FALSE.
#' @noRd
.hal_setup_finish <- function(quiet) {
  # Verify
  if (!hal_available()) {
    cli::cli_alert_warning(
      "CLI reports ready but verification failed. Restart your IDE and try again."
    )
    return(invisible(FALSE))
  }

  version <- .hal_cli_version()
  if (!quiet && !is.null(version)) {
    cli::cli_alert_success("Copilot CLI verified: {version}")
  }

  # Authentication
  if (!quiet) {
    cli::cli_h2("Authentication")
    cli_info <- tryCatch(find_hal_cli(), error = function(e) NULL)
    if (!is.null(cli_info) && length(cli_info$prefix_args) > 0) {
      # gh copilot path
      cli::cli_alert_info(
        "Run {.code gh auth login} in your terminal to authenticate with GitHub."
      )
    } else {
      cli::cli_alert_info(
        "Run {.code copilot login} in your terminal to authenticate with GitHub."
      )
    }
    cli::cli_bullets(c(
      "i" = "This opens a browser for GitHub device flow authentication.",
      "i" = "You need a GitHub Copilot subscription (Free tier works).",
      "i" = "After login, credentials are cached automatically."
    ))
  }

  # Offer to run login interactively
  if (interactive()) {
    cli_info <- tryCatch(find_hal_cli(), error = function(e) NULL)
    if (!is.null(cli_info)) {
      # Build the login command based on which CLI we found
      if (length(cli_info$prefix_args) > 0) {
        login_prompt <- "Run `gh auth login` now? (y/n): "
        login_args <- c("auth", "login")
        login_cmd <- cli_info$command
      } else {
        login_prompt <- "Run `copilot login` now? (y/n): "
        login_args <- c(cli_info$prefix_args, "login")
        login_cmd <- cli_info$command
      }

      run_login <- tryCatch(
        {
          response <- readline(login_prompt)
          tolower(trimws(response)) %in% c("y", "yes")
        },
        error = function(e) FALSE
      )
      if (run_login) {
        cli::cli_alert_info("Follow the prompts in your browser.")
        tryCatch(
          processx::run(
            login_cmd, login_args,
            echo = TRUE, spinner = TRUE,
            timeout = 120, error_on_status = FALSE
          ),
          error = function(e) {
            cli::cli_alert_warning("Login process error: {e$message}")
          }
        )
      }
    }
  }

  if (!quiet) {
    cli::cli_h2("Ready")
    cli::cli_alert_success("Setup complete. Try: {.code hal(\"Hello!\")}")
  }

  invisible(TRUE)
}


# ==============================================================================
# Claude branch -- Claude Code CLI setup
# ==============================================================================

#' Set up the Claude Code backend
#'
#' Detect-and-instruct rather than auto-install, unlike the Copilot path.
#' Two reasons: Claude Code's sign-in is an interactive OAuth flow that cannot
#' be driven from R, and installing it through npm yields the `.cmd` shim
#' whose stdio gets dropped under processx -- the exact problem
#' `.hal_prefer_native_claude()` exists to route around. Pointing users at the
#' native installer avoids manufacturing that situation.
#'
#' @param force Re-check even if the CLI already reports as working.
#' @param quiet Suppress progress messages.
#' @return Invisibly `TRUE` if the CLI is present and runnable.
#' @noRd
.hal_setup_claude <- function(force = FALSE, quiet = FALSE) {
  if (!force && hal_available(backend = "claude")) {
    cli::cli_alert_success("Claude Code CLI is already installed and working.")
    version <- .hal_claude_cli_version()
    if (!is.null(version)) cli::cli_alert_info("Version: {version}")
    if (!quiet) {
      cli::cli_alert_info(
        "Select it with {.code hal_configure(backend = \"claude\")}."
      )
      cli::cli_alert_info("Run {.code hal_setup(force = TRUE)} to re-check.")
    }
    return(invisible(TRUE))
  }

  if (!quiet) cli::cli_h1("hal setup (Claude Code)")

  info <- tryCatch(find_claude_cli(), error = function(e) NULL)
  if (is.null(info)) {
    cli::cli_alert_danger("Claude Code CLI not found.")
    .hal_claude_install_hint()
    return(invisible(FALSE))
  }

  if (!quiet) cli::cli_alert_success("Found: {.path {info$command}}")

  version <- .hal_claude_cli_version()
  if (is.null(version)) {
    cli::cli_alert_warning(
      "The {.code claude} binary was found but did not report a version."
    )
    cli::cli_bullets(c(
      "i" = "Run {.code claude --version} in a terminal to see the error.",
      "i" = "If it asks you to sign in, run {.code claude} once and complete OAuth."
    ))
    return(invisible(FALSE))
  }

  if (!quiet) cli::cli_alert_success("Claude Code CLI verified: {version}")

  # Sign-in cannot be probed from here: `claude --version` succeeds whether or
  # not OAuth has been completed. Surface it as a reminder instead of a check.
  if (!quiet) {
    cli::cli_h2("Authentication")
    cli::cli_bullets(c(
      "i" = "hal reuses your existing Claude.ai subscription sign-in.",
      "i" = "Not signed in yet? Run {.code claude} in a terminal once and complete the browser flow.",
      "i" = "hal cannot verify sign-in from R -- the first {.code hal()} call will surface an auth error if it is missing."
    ))

    cli::cli_h2("Ready")
    cli::cli_alert_info(
      "Select the backend: {.code hal_configure(backend = \"claude\")}"
    )
    cli::cli_alert_success("Then try: {.code hal(\"Hello!\")}")
  }

  invisible(TRUE)
}

#' Platform-appropriate Claude Code install instructions.
#' @return Invisibly `NULL`, called for its side effect.
#' @noRd
.hal_claude_install_hint <- function() {
  cli::cli_alert_info(
    "Install Claude Code, then run {.code hal_setup(backend = \"claude\")} again:"
  )
  cli::cli_bullets(c(
    "i" = "Download: {.url https://claude.ai/download}",
    "i" = "Then run {.code claude} once in a terminal to complete sign-in."
  ))
  cli::cli_alert_info(
    "Already installed elsewhere? Set {.envvar CLAUDE_CLI_PATH} to the binary."
  )
  cli::cli_alert_warning("Restart your IDE (Positron/RStudio) after installing.")
  invisible(NULL)
}


# ==============================================================================
# Positron branch -- vscode backend setup
# ==============================================================================

#' Positron-specific setup: install the bundled hal-bridge extension.
#'
#' The VSIX ships inside hal at inst/extdata/ -- there is no download and
#' no GitHub CLI involvement. The only external requirement is a Copilot
#' sign-in inside Positron itself, which `vscode.lm` uses for model access;
#' hal cannot verify that from R, so we surface it as a reminder rather
#' than a gate.
#'
#' Steps:
#' 1. Verify Positron context (already done by caller).
#' 2. Install / verify the hal-bridge extension from the bundled VSIX.
#' 3. Remind about the cold restart + Copilot sign-in.
#'
#' @keywords internal
#' @noRd
.hal_setup_positron <- function(force = FALSE, quiet = FALSE) {
  cli::cli_h1("hal setup (Positron)")
  if (!quiet) {
    cli::cli_alert_info(c(
      "Positron detected -- the vscode backend skips the Copilot CLI entirely.",
      " " = "Model traffic goes through the hal-bridge extension to vscode.lm."
    ))
  }

  # Fast path: bridge already installed and reachable?
  if (!force && hal_available(backend = "vscode")) {
    if (!quiet) cli::cli_alert_success("hal-bridge is installed and running.")
    hal_bridge_status()
    if (!quiet) {
      cli::cli_alert_info(
        "Try: {.code options(hal.backend = 'vscode'); hal('Hello!')}"
      )
    }
    return(invisible(TRUE))
  }

  # Offer to install the bridge.
  if (interactive() && !force) {
    response <- tryCatch(
      readline("Install hal-bridge extension now? [Y/n]: "),
      error = function(e) ""
    )
    if (tolower(trimws(response)) %in% c("n", "no")) {
      cli::cli_alert_info(
        "Skipped. Run {.code hal_install_bridge()} when ready."
      )
      return(invisible(FALSE))
    }
  }

  installed <- tryCatch(
    {
      hal_install_bridge(force = isTRUE(force))
      TRUE
    },
    error = function(e) {
      cli::cli_alert_danger("hal-bridge install failed: {conditionMessage(e)}")
      FALSE
    }
  )
  if (!isTRUE(installed)) return(invisible(FALSE))

  cli::cli_h2("Next step")
  cli::cli_bullets(c(
    "i" = "Fully quit Positron and reopen it -- not just 'Reload Window', and not just restarting R. New extensions only load on a cold start.",
    "i" = "Make sure you are signed in to GitHub Copilot in Positron (account menu, lower left). The bridge uses that sign-in for model access -- no API keys, no gh CLI.",
    "i" = "After Positron is back up, verify: {.run hal_bridge_status()}",
    "i" = "Then try:  {.code hal('Hello!')}"
  ))
  invisible(TRUE)
}


#' Get the Claude Code CLI version string
#' @return Character string or NULL.
#' @noRd
.hal_claude_cli_version <- function() {
  tryCatch({
    info <- find_claude_cli()
    result <- processx::run(info$command, "--version",
                            timeout = 10, error_on_status = FALSE)
    if (result$status != 0L) return(NULL)
    ver <- trimws(result$stdout)
    if (!grepl("[0-9]", ver)) return(NULL)
    ver
  }, error = function(e) NULL)
}


#' Get the Copilot CLI version string
#' @return Character string or NULL.
#' @noRd
.hal_cli_version <- function() {
  tryCatch({
    info <- find_hal_cli()
    args <- c(info$prefix_args, "--version")
    result <- processx::run(info$command, args,
                            timeout = 10, error_on_status = FALSE)
    if (result$status != 0L) return(NULL)
    ver <- trimws(result$stdout)
    # Sanity check: a version string should contain at least one digit
    if (!grepl("[0-9]", ver)) return(NULL)
    ver
  }, error = function(e) NULL)
}
