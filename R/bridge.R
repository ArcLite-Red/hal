# ==============================================================================
# hal-bridge: installer, discovery, and status for the Positron extension
# ==============================================================================
#
# The vscode backend talks to a small VS Code / Positron extension
# (hal-bridge) over localhost HTTP. This file contains everything related to
# locating, installing, and probing that extension. The R6 transport client
# lives in client-vscode.R and uses the discovery helpers from here.

# ------------------------------------------------------------------------------
# Bundled VSIX
# ------------------------------------------------------------------------------
# The hal-bridge VSIX ships inside this package at inst/extdata/. To bump the
# bridge, drop a new VSIX into inst/extdata/ and update BRIDGE_VERSION to
# match. No GitHub download, no SHA pin, no auth dance -- the bytes users
# install are the bytes shipped with the hal version they installed.

BRIDGE_VERSION <- "0.1.4"

#' Path to the bundled hal-bridge VSIX.
#' @keywords internal
#' @noRd
.hal_bundled_vsix <- function(version = BRIDGE_VERSION) {
  asset <- sprintf("extdata/hal-bridge-%s.vsix", version)
  path <- system.file(asset, package = "hal")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort(c(
      "Bundled hal-bridge VSIX not found at {.path inst/{asset}}.",
      "i" = "This hal install is missing its bridge asset -- please reinstall hal."
    ))
  }
  path
}

# ------------------------------------------------------------------------------
# Discovery (port file + bridge probe)
# ------------------------------------------------------------------------------

#' Path to the bridge discovery file (durable per-user app-data dir).
#'
#' The extension writes its listening port (and a per-launch bearer token) to
#' this file on activation. The location must match `portFileDir()` in the
#' bridge's `extension.ts`:
#'   Windows: `%LOCALAPPDATA%\\hal-bridge\\port.json`
#'   POSIX:   `$XDG_RUNTIME_DIR/hal-bridge/port.json`, else
#'            `~/.cache/hal-bridge/port.json`
#'
#' This deliberately avoids the system temp dir, which the OS garbage-collects
#' (e.g. Windows Storage Sense) and would orphan a still-running bridge.
#' @keywords internal
#' @noRd
.hal_bridge_port_file <- function() {
  if (.Platform$OS.type == "windows") {
    base <- Sys.getenv("LOCALAPPDATA", unset = Sys.getenv("TEMP", unset = tempdir()))
    return(file.path(base, "hal-bridge", "port.json"))
  }
  base <- Sys.getenv("XDG_RUNTIME_DIR", unset = "")
  if (!nzchar(base)) {
    base <- file.path(Sys.getenv("HOME", unset = path.expand("~")), ".cache")
  }
  file.path(base, "hal-bridge", "port.json")
}

#' Read and validate the bridge port file.
#'
#' @return List with `port`, `pid`, `version`, `started`.
#' @keywords internal
#' @noRd
.hal_bridge_discover <- function() {
  pf <- .hal_bridge_port_file()
  if (!file.exists(pf)) {
    cli::cli_abort(c(
      "hal-bridge port file not found at {.path {pf}}.",
      "i" = "Install the extension with {.run hal_install_bridge()}, then fully quit and reopen Positron (a 'Reload Window' is not enough on a fresh install).",
      "i" = "If already installed, the bridge may have failed to start -- check the Extension Host log."
    ))
  }
  info <- tryCatch(
    jsonlite::read_json(pf),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to parse bridge port file {.path {pf}}: {conditionMessage(e)}"
      ))
    }
  )
  if (is.null(info$port) || !is.numeric(info$port)) {
    cli::cli_abort("Bridge port file is malformed (missing port).")
  }
  info
}

# ------------------------------------------------------------------------------
# Environment detection
# ------------------------------------------------------------------------------

#' Detect whether we're running inside Positron.
#'
#' Positron sets `POSITRON_VERSION` for hosted R sessions. Useful to decide
#' whether to offer the vscode backend during setup.
#'
#' @return Logical.
#' @keywords internal
#' @noRd
.is_positron <- function() nzchar(Sys.getenv("POSITRON_VERSION"))

#' Locate the Positron CLI binary.
#'
#' Searches PATH, common platform install locations, and the `POSITRON_BIN`
#' env var override. Returns a normalized absolute path or aborts with
#' actionable guidance.
#'
#' @return Character path.
#' @keywords internal
#' @noRd
.find_positron_cli <- function() {
  # Native separators (backslash on Windows): the Positron CLI is a .cmd shim,
  # so processx routes it through cmd.exe, which mis-tokenizes forward-slash
  # paths containing spaces (e.g. "C:/Program Files/...") -> status 1.
  ws <- if (.Platform$OS.type == "windows") "\\" else "/"

  override <- Sys.getenv("POSITRON_BIN", unset = NA_character_)
  if (!is.na(override) && nzchar(override) && file.exists(override)) {
    return(normalizePath(override, winslash = ws, mustWork = TRUE))
  }

  on_path <- Sys.which(if (.Platform$OS.type == "windows") "positron.cmd" else "positron")
  if (nzchar(on_path)) {
    return(normalizePath(on_path, winslash = ws, mustWork = TRUE))
  }

  candidates <- if (.Platform$OS.type == "windows") {
    c(
      file.path(Sys.getenv("ProgramFiles"), "Positron", "bin", "positron.cmd"),
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Positron", "bin", "positron.cmd")
    )
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    c(
      "/Applications/Positron.app/Contents/Resources/app/bin/positron",
      "~/Applications/Positron.app/Contents/Resources/app/bin/positron"
    )
  } else {
    c("/usr/bin/positron", "/usr/local/bin/positron", "/snap/bin/positron")
  }
  candidates <- path.expand(candidates)
  found <- candidates[file.exists(candidates)]
  if (length(found)) {
    return(normalizePath(found[1], winslash = ws, mustWork = TRUE))
  }

  cli::cli_abort(c(
    "Cannot locate the Positron CLI.",
    "i" = "Tried PATH and: {.path {candidates}}.",
    "i" = "Set {.envvar POSITRON_BIN} to override, or install Positron from {.url https://positron.posit.co}."
  ))
}

# ------------------------------------------------------------------------------
# Install
# ------------------------------------------------------------------------------

#' Install the hal-bridge Positron extension
#'
#' Installs the hal-bridge VSIX shipped with this hal release into Positron.
#' No download, no GitHub auth, no SHA verification: the bytes installed are
#' the bytes that shipped in `inst/extdata/`. Reload Positron after
#' installing to activate the bridge.
#'
#' The vscode backend (`hal_configure(backend = "vscode")`) requires this
#' extension. Other backends (`copilot`, `claude`) do not.
#'
#' @param local_path Optional path to a `.vsix` to install instead of the
#'   bundled one. Useful for testing a dev build of the bridge.
#' @param force If `TRUE`, pass `--force` to the Positron CLI so an existing
#'   install is replaced.
#'
#' @return Invisibly returns the installed VSIX path.
#'
#' @seealso [hal_bridge_status()] to check whether it's actually running.
#'
#' @examples
#' \dontrun{
#' hal_install_bridge()
#' hal_install_bridge(local_path = "~/Downloads/hal-bridge-dev.vsix")
#' }
#' @export
hal_install_bridge <- function(local_path = NULL, force = FALSE) {
  if (!.is_positron()) {
    cli::cli_warn(c(
      "!" = "{.fn hal_install_bridge} is intended for Positron sessions.",
      "i" = "{.envvar POSITRON_VERSION} is not set; continuing anyway."
    ))
  }

  vsix <- if (!is.null(local_path)) {
    normalizePath(local_path, mustWork = TRUE)
  } else {
    .hal_bundled_vsix()
  }

  positron <- .find_positron_cli()
  args <- c("--install-extension", vsix)
  if (isTRUE(force)) args <- c(args, "--force")

  cli::cli_inform(c("i" = "Installing {.path {basename(vsix)}} via {.path {positron}}..."))
  # The Positron CLI is a .cmd batch shim. processx auto-wraps it in cmd.exe but
  # pastes the path unquoted, so a path with spaces ("C:\Program Files\...") is
  # split at the space -> "'C:\Program' is not recognized". Drive cmd.exe
  # ourselves: as discrete args, processx quotes the path (and the VSIX path),
  # and the leading `call` defeats cmd.exe's outer-quote-stripping quirk.
  if (.Platform$OS.type == "windows") {
    res <- processx::run(
      Sys.getenv("COMSPEC", "cmd.exe"),
      args = c("/c", "call", positron, args),
      error_on_status = FALSE
    )
  } else {
    res <- processx::run(positron, args = args, error_on_status = FALSE)
  }
  if (res$status != 0L) {
    cli::cli_abort(c(
      "Positron CLI returned status {res$status}.",
      "x" = if (nzchar(res$stderr)) res$stderr else res$stdout
    ))
  }
  cli::cli_inform(c(
    "v" = "hal-bridge {BRIDGE_VERSION} installed.",
    "i" = "Fully quit Positron and reopen it -- {.emph not} just an R session restart, and not just 'Reload Window'. The extension host loads new extensions on a cold start.",
    "i" = "After Positron is back up, verify with {.run hal_bridge_status()}."
  ))
  invisible(vsix)
}

# ------------------------------------------------------------------------------
# Authenticated GET against the bridge (used by /models and other JSON routes)
# ------------------------------------------------------------------------------

#' GET a JSON document from the bridge, optionally with a bearer token.
#'
#' Forward-compatible: an older bridge that doesn't require auth simply
#' ignores the extra header; a newer one rejects requests missing it.
#'
#' @keywords internal
#' @noRd
.hal_bridge_get_json <- function(port, path, token = NULL) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg curl} is required.")
  }
  url <- sprintf("http://127.0.0.1:%d%s", port, path)
  h <- curl::new_handle()
  headers <- c("Accept" = "application/json")
  if (!is.null(token) && nzchar(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }
  do.call(curl::handle_setheaders, c(list(handle = h), as.list(headers)))
  resp <- curl::curl_fetch_memory(url, handle = h)
  if (resp$status_code >= 400L) {
    msg <- switch(as.character(resp$status_code),
      "401" = "Bridge rejected our token. Reload Positron to rotate it.",
      "429" = "Bridge is at its concurrency limit. Wait and retry.",
      paste0("Bridge returned HTTP ", resp$status_code, ".")
    )
    cli::cli_abort(c("hal-bridge HTTP {resp$status_code}", "x" = msg))
  }
  jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
}


# ------------------------------------------------------------------------------
# Status
# ------------------------------------------------------------------------------

#' Check whether hal-bridge is installed and running
#'
#' Reads the bridge port file, pings the `/version` endpoint, and warns on
#' version drift between the running bridge and the version this hal release
#' was pinned against.
#'
#' @return Invisibly returns `TRUE` if the bridge is reachable, `FALSE`
#'   otherwise. Always prints a status line as a side effect.
#'
#' @seealso [hal_install_bridge()].
#'
#' @examples
#' \dontrun{
#' hal_bridge_status()
#' }
#' @export
hal_bridge_status <- function() {
  pf <- .hal_bridge_port_file()
  if (!file.exists(pf)) {
    cli::cli_alert_warning(c(
      "hal-bridge port file not found at {.path {pf}}.",
      "i" = "Run {.run hal_install_bridge()} if the extension isn't installed,",
      "i" = "or reload Positron if it is installed but inactive."
    ))
    return(invisible(FALSE))
  }
  info <- tryCatch(jsonlite::read_json(pf), error = function(e) NULL)
  if (is.null(info$port)) {
    cli::cli_alert_warning("Bridge port file is malformed.")
    return(invisible(FALSE))
  }

  url <- sprintf("http://127.0.0.1:%d/version", info$port)
  ver <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = TRUE)$version,
    error = function(e) NULL
  )
  if (is.null(ver)) {
    cli::cli_alert_warning(c(
      "Port file exists but the bridge is not responding on port {info$port}.",
      "i" = "It may have crashed -- reload Positron to restart it."
    ))
    return(invisible(FALSE))
  }

  if (!identical(ver, BRIDGE_VERSION)) {
    cli::cli_alert_warning(c(
      "Bridge version mismatch: running {.val {ver}}, hal expects {.val {BRIDGE_VERSION}}.",
      "i" = "Run {.run hal_install_bridge(force = TRUE)} to update."
    ))
  } else {
    cli::cli_alert_success(
      "hal-bridge {.val {ver}} running on port {info$port}."
    )
  }
  invisible(TRUE)
}
