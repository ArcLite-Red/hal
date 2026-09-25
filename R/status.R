# ==============================================================================
# hal_status -- One-look diagnostic for the whole stack
# ==============================================================================
#
# Consolidates what a user would otherwise piece together from
# hal_available(), hal_bridge_status(), hal_config(), and hal(inspect = TRUE)
# into a single traffic-light report that always ends with a concrete next
# step. This is the function to reach for when "hal doesn't work".

#' Diagnose your hal installation in one call
#'
#' Prints a traffic-light report covering everything between you and a
#' working `hal()` call: which backend is selected (and why), whether its
#' transport is reachable, what the active session looks like, and -- when
#' something is wrong -- the single next step to fix it.
#'
#' This is the first thing to run when hal misbehaves, and the thing to
#' paste into a bug report.
#'
#' @return Invisibly, a list with elements `backend`, `backend_source`,
#'   `available`, `detail`, `session_active`, `model`, and `next_step`
#'   (`NULL` when everything is ready).
#'
#' @seealso [hal_setup()], [hal_available()], [hal_bridge_status()],
#'   [hal_config()]
#'
#' @export
#' @examples
#' \dontrun{
#' hal_status()
#' }
hal_status <- function() {
  cli::cli_h1("hal status")

  # ---- Package + backend resolution -----------------------------------------
  version <- as.character(utils::packageVersion("hal"))
  backend_opt <- getOption("hal.backend", NULL)
  backend <- tryCatch(.hal_backend(), error = function(e) NA_character_)
  backend_source <- if (!is.null(backend_opt) && nzchar(backend_opt)) {
    "hal.backend option"
  } else if (.is_positron()) {
    "auto (Positron detected)"
  } else {
    "auto (default)"
  }

  cli::cli_alert_info("hal {version} | backend: {.val {backend}} ({backend_source})")

  if (is.na(backend)) {
    next_step <- "Set a valid backend: hal_configure(backend = \"vscode\"|\"copilot\"|\"claude\")"
    cli::cli_alert_danger("Backend option is invalid.")
    cli::cli_alert_info("Next step: {.code {next_step}}")
    return(invisible(list(
      backend = NA_character_, backend_source = backend_source,
      available = FALSE, detail = "invalid backend option",
      session_active = FALSE, model = NULL, next_step = next_step
    )))
  }

  # ---- Transport check per backend -------------------------------------------
  probe <- switch(backend,
    vscode  = .hal_status_vscode(),
    claude  = .hal_status_cli("claude"),
    copilot = .hal_status_cli("copilot")
  )

  if (isTRUE(probe$ok)) {
    cli::cli_alert_success(probe$detail)
  } else {
    cli::cli_alert_danger(probe$detail)
  }

  # ---- Session state ----------------------------------------------------------
  session <- tryCatch(.hal_get_session(), error = function(e) NULL)
  session_active <- !is.null(session) && !is.null(session$chat)
  model <- if (session_active) {
    session$model
  } else {
    getOption("hal.default_model", NULL)
  }

  if (session_active) {
    n_turns <- tryCatch(length(session$chat$get_turns()), error = function(e) NA_integer_)
    cli::cli_alert_success(
      "Session active | model: {.val {model %||% 'backend default'}} | turns: {n_turns}"
    )
  } else {
    cli::cli_alert_info(
      "No active session (one starts on your first {.code hal()} call). Model: {.val {model %||% 'backend default'}}"
    )
  }

  # ---- Verdict ----------------------------------------------------------------
  next_step <- if (isTRUE(probe$ok)) NULL else probe$next_step
  if (is.null(next_step)) {
    cli::cli_alert_success("Ready. Try: {.code hal(\"Hello!\")}")
  } else {
    cli::cli_alert_info("Next step: {.code {next_step}}")
  }

  invisible(list(
    backend = backend,
    backend_source = backend_source,
    available = isTRUE(probe$ok),
    detail = probe$detail,
    session_active = session_active,
    model = model,
    next_step = next_step
  ))
}

# ------------------------------------------------------------------------------
# Per-backend probes: each returns list(ok, detail, next_step)
# ------------------------------------------------------------------------------

#' Probe the vscode backend (hal-bridge over localhost HTTP)
#' @keywords internal
#' @noRd
.hal_status_vscode <- function() {
  if (!.is_positron()) {
    return(list(
      ok = FALSE,
      detail = "Backend is 'vscode' but this R session is not inside Positron.",
      next_step = "hal_configure(backend = \"copilot\")  # or run this session in Positron"
    ))
  }

  pf <- .hal_bridge_port_file()
  if (!file.exists(pf)) {
    return(list(
      ok = FALSE,
      detail = "hal-bridge extension is not running (no port file).",
      next_step = "hal_setup()  # installs the bundled bridge; then fully restart Positron"
    ))
  }

  info <- tryCatch(jsonlite::read_json(pf), error = function(e) NULL)
  if (is.null(info$port)) {
    return(list(
      ok = FALSE,
      detail = "Bridge port file is malformed.",
      next_step = "hal_install_bridge(force = TRUE)  # then fully restart Positron"
    ))
  }

  ver <- tryCatch(
    jsonlite::fromJSON(
      sprintf("http://127.0.0.1:%d/version", info$port),
      simplifyVector = TRUE
    )$version,
    error = function(e) NULL
  )
  if (is.null(ver)) {
    return(list(
      ok = FALSE,
      detail = sprintf(
        "Bridge port file exists but nothing is answering on port %d.", info$port
      ),
      next_step = "Fully quit and reopen Positron (the bridge restarts on a cold start)"
    ))
  }

  if (!identical(ver, BRIDGE_VERSION)) {
    return(list(
      ok = TRUE,
      detail = sprintf(
        "hal-bridge %s responding on port %d (hal expects %s -- consider updating).",
        ver, info$port, BRIDGE_VERSION
      ),
      next_step = NULL
    ))
  }

  list(
    ok = TRUE,
    detail = sprintf("hal-bridge %s responding on port %d.", ver, info$port),
    next_step = NULL
  )
}

#' Probe a CLI backend (copilot or claude)
#' @keywords internal
#' @noRd
.hal_status_cli <- function(backend) {
  finder <- if (identical(backend, "claude")) find_claude_cli else find_hal_cli
  label <- if (identical(backend, "claude")) "Claude Code CLI" else "Copilot CLI"
  login_hint <- if (identical(backend, "claude")) {
    "claude  # then /login in the CLI"
  } else {
    "hal_setup()  # installs and walks through login"
  }

  info <- tryCatch(finder(), error = function(e) NULL)
  if (is.null(info)) {
    # Copilot missing but Claude Code present: switching backend is the one
    # step that gets a working hal() now, so make it the step we name.
    if (!identical(backend, "claude") && .hal_claude_installed()) {
      return(list(
        ok = FALSE,
        detail = sprintf("%s not found on PATH (Claude Code is installed).", label),
        next_step = 'hal_configure(backend = "claude")  # or hal_setup() to install Copilot'
      ))
    }
    return(list(
      ok = FALSE,
      detail = sprintf("%s not found on PATH.", label),
      next_step = login_hint
    ))
  }

  result <- tryCatch(
    processx::run(info$command, c(info$prefix_args, "--version"),
                  timeout = 10, error_on_status = FALSE),
    error = function(e) NULL
  )
  if (is.null(result) || result$status != 0L || !grepl("[0-9]", result$stdout)) {
    return(list(
      ok = FALSE,
      detail = sprintf("%s found at %s but not responding to --version.",
                       label, info$command),
      next_step = login_hint
    ))
  }

  list(
    ok = TRUE,
    detail = sprintf("%s %s at %s", label,
                     trimws(strsplit(result$stdout, "\n")[[1]][1]),
                     info$command),
    next_step = NULL
  )
}
