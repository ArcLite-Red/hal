#' Find the Copilot CLI binary
#'
#' The Copilot CLI can be invoked in two ways:
#' 1. Standalone binary: `copilot --acp`
#' 2. Via gh: `gh copilot -- --acp`
#'
#' Returns a list with `command` and `prefix_args` to handle both cases.
#'
#' @param cli_path Optional explicit path. If provided, used directly.
#' @return A list with `command` (string) and `prefix_args` (character vector).
#' @noRd
find_hal_cli <- function(cli_path = NULL) {
  # Explicit path provided

  if (!is.null(cli_path)) {
    if (!file.exists(cli_path)) {
      cli::cli_abort(c(
        "Copilot CLI not found at specified path.",
        "x" = "File does not exist: {.path {cli_path}}"
      ))
    }
    return(list(command = cli_path, prefix_args = character()))
  }

  # Check COPILOT_CLI_PATH env var
  env_path <- Sys.getenv("COPILOT_CLI_PATH", "")
  if (nzchar(env_path) && file.exists(env_path)) {
    return(list(command = env_path, prefix_args = character()))
  }

  # Check for standalone `copilot` binary on PATH
  hal_path <- Sys.which("copilot")
  if (nzchar(hal_path)) {
    return(list(command = as.character(hal_path), prefix_args = character()))
  }

  # Check for `gh` with copilot subcommand

  gh_path <- Sys.which("gh")
  if (nzchar(gh_path)) {
    # Verify gh copilot is available
    check <- tryCatch(
      processx::run(as.character(gh_path), c("copilot", "--", "--version"),
                    timeout = 10, error_on_status = FALSE),
      error = function(e) list(status = 1L)
    )
    if (check$status == 0L) {
      return(list(
        command = as.character(gh_path),
        prefix_args = c("copilot", "--")
      ))
    }
  }

  # Platform-specific common locations
  if (.Platform$OS.type == "windows") {
    local_app <- Sys.getenv("LOCALAPPDATA", "")
    candidates <- character()
    if (nzchar(local_app)) {
      candidates <- c(
        file.path(local_app, "GitHub CLI", "copilot.exe"),
        file.path(local_app, "GitHub", "copilot-cli", "copilot.exe"),
        file.path(local_app, "Programs", "copilot", "copilot.exe")
      )
    }
  } else {
    candidates <- c(
      "/usr/local/bin/copilot",
      file.path(Sys.getenv("HOME"), ".local", "bin", "copilot"),
      "/opt/homebrew/bin/copilot"
    )
  }

  for (cand in candidates) {
    if (file.exists(cand)) {
      return(list(command = cand, prefix_args = character()))
    }
  }

  cli::cli_abort(.hal_copilot_missing_msg())
}

#' Message for a missing Copilot CLI
#'
#' Leads with the Claude backend when Claude Code is already installed: for
#' that user, switching is one command away, while installing Copilot is not.
#' @return A cli message vector.
#' @keywords internal
#' @noRd
.hal_copilot_missing_msg <- function() {
  install_hint <- if (.Platform$OS.type == "windows") {
    "Install via: {.code winget install GitHub.cli} then {.code gh extension install github/gh-copilot}"
  } else {
    "Install via: {.code gh extension install github/gh-copilot} (requires GitHub CLI)"
  }
  c(
    "Could not find the Copilot CLI.",
    .hal_claude_hint(),
    "i" = install_hint,
    "i" = "Or run {.code hal_setup()} for guided installation.",
    "i" = "Or set {.envvar COPILOT_CLI_PATH} to the binary path."
  )
}

#' Is the Claude Code CLI installed?
#'
#' Cheap by design -- `CLAUDE_CLI_PATH` and the PATH only, no subprocess -- so
#' it is safe to call from error paths.
#' @return Logical.
#' @keywords internal
#' @noRd
.hal_claude_installed <- function() {
  !is.null(tryCatch(find_claude_cli(), error = function(e) NULL))
}

#' "Use Claude instead" hint, as a cli bullet; empty if Claude isn't installed
#' @keywords internal
#' @noRd
.hal_claude_hint <- function() {
  if (!.hal_claude_installed()) return(character())
  c("i" = "Claude Code is already installed -- to use it instead: {.code hal_configure(backend = \"claude\")}")
}

#' Check whether the active backend's transport is ready
#'
#' Dispatches by backend:
#' - `vscode`: bridge port file exists and `/version` responds.
#' - `copilot`: Copilot CLI binary found and `--version` succeeds.
#' - `claude`: Claude CLI binary found and `--version` succeeds.
#'
#' Honors the `hal.backend` option / explicit argument so callers can probe
#' a specific backend without switching the session default.
#'
#' @param backend Optional backend id (`"vscode"`, `"copilot"`, `"claude"`).
#'   If `NULL`, uses the resolved default for this session.
#' @return `TRUE` if the backend's transport is reachable, `FALSE` otherwise.
#'
#' @seealso [HalChat], [hal_models()], [hal_bridge_status()] for richer
#'   vscode-backend diagnostics.
#'
#' @examples
#' hal_available()
#' \dontrun{
#' hal_available(backend = "vscode")
#' hal_available(backend = "claude")
#' }
#'
#' @export
hal_available <- function(backend = NULL) {
  b <- tryCatch(.hal_backend(backend), error = function(e) NULL)
  if (is.null(b)) return(FALSE)

  if (identical(b, "vscode")) {
    pf <- .hal_bridge_port_file()
    if (!file.exists(pf)) return(FALSE)
    info <- tryCatch(jsonlite::read_json(pf), error = function(e) NULL)
    if (is.null(info$port)) return(FALSE)
    ok <- tryCatch(
      {
        url <- sprintf("http://127.0.0.1:%d/version", info$port)
        ver <- jsonlite::fromJSON(url, simplifyVector = TRUE)$version
        !is.null(ver) && nzchar(ver)
      },
      error = function(e) FALSE
    )
    return(isTRUE(ok))
  }

  if (identical(b, "claude")) {
    return(tryCatch({
      info <- find_claude_cli()
      result <- processx::run(info$command,
                              c(info$prefix_args, "--version"),
                              timeout = 10, error_on_status = FALSE)
      result$status == 0L && grepl("[0-9]", result$stdout)
    }, error = function(e) FALSE))
  }

  # copilot
  tryCatch({
    info <- find_hal_cli()
    result <- processx::run(info$command, c(info$prefix_args, "--version"),
                            timeout = 10, error_on_status = FALSE)
    result$status == 0L && grepl("[0-9]", result$stdout)
  }, error = function(e) FALSE)
}
