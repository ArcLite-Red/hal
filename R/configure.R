# ==============================================================================
# hal Configuration -- Display, governance, and session options
# ==============================================================================
#
# Ported from HAL_configure. Trimmed: no provider factory (single backend),
# no explore_groups (no btw dependency). Options use hal.* prefix.

#' Configure hal Options
#'
#' Set display, governance, and session defaults. Settings persist for the
#' duration of the R session via `options()`.
#'
#' @param use_colors Logical; enable ANSI color output (default: TRUE).
#' @param stream Logical; enable streaming typewriter output (default: TRUE).
#' @param stream_speed Character; `"instant"`, `"fast"`, `"medium"`, or
#'   `"slow"` (default: `"medium"`).
#' @param default_model Character; default model identifier.
#' @param system_prompt Character; custom system prompt. Use `NULL` to keep
#'   the current prompt. The placeholder `{user}` is replaced with the
#'   current login name.
#' @param eval_denylist Character vector of function names blocked from
#'   `eval_r` execution. Set to `FALSE` to disable. Default: curated list.
#' @param credential_action Character; action on credential detection:
#'   `"warn"` (default), `"redact"`, or `"block"`.
#' @param eval_timeout Numeric; seconds for eval_r time limit (default: 30).
#' @param prompt_timeout Numeric; seconds before aborting a resumed prompt
#'   on the Claude backend (default: 60). Has no effect on Copilot.
#' @param prompt_timeout_cold Numeric; seconds before aborting the first
#'   (session-creating) prompt on the Claude backend (default: 180). The
#'   cold path pays for Node.js startup, AV scanning, and session
#'   provisioning on Windows and can exceed the warm timeout.
#' @param edit_in_place Logical; when TRUE, `hal_do()` (and `hal_excel()`)
#'   replaces itself in the editor with generated code. When unset, defaults
#'   to TRUE where applicable -- i.e. when called from a saved IDE source
#'   editor buffer -- and FALSE from the console. Set explicitly to force
#'   either behaviour.
#' @param do_retries Integer; max retry attempts for `hal_do()` when generated
#'   code fails to parse or execute (default: 2). Set to 0 to disable.
#' @param do_on_fail Character; what `hal_do()` does when generation fails
#'   after all retries: `"warn"` warns and passes `.data` through unchanged;
#'   `"abort"` raises an error. When unset, defaults to `"warn"` in
#'   interactive sessions and `"abort"` in non-interactive contexts
#'   (scripts, R Markdown, `targets`), where silently continuing with
#'   untransformed data is worse than failing.
#' @param verify Logical; when TRUE (default), `hal_do()` pipe mode compares
#'   input and output data frames and displays a one-line structural report,
#'   attaching the full report as `attr(result, "hal_verify")`. Report-only:
#'   never affects retries or values.
#' @param plot_vision Logical; when TRUE (default), plots drawn by `eval_r`
#'   are captured as PNG and returned to the model as images so it can see
#'   and iterate on them (vscode backend with hal-bridge >= 0.1.4, and
#'   claude backend). The copilot backend is always text-only regardless of
#'   this setting (its CLI's image forwarding is unverified). Notes: plots
#'   written to file devices the code opens itself (`png()`, `pdf()`) are
#'   not echoed; one image per eval (the final page); worst-case eval_r
#'   time is ~2x `eval_timeout` (eval + render).
#' @param permission_policy Character or function; how to handle tool
#'   permission requests. `"auto-allow"` (default) auto-approves;
#'   `"auto-deny"` blocks every tool call; `"ask"` prompts interactively on
#'   the vscode backend (denies in non-interactive sessions); a function
#'   receives `list(backend, tool_name, input)` (copilot adds the full ACP
#'   params) and returns a decision -- a string containing `"allow"` or
#'   `"deny"` (vscode/claude), or an ACP option id (copilot). Takes effect
#'   on next session init (call `hal_reset()` to apply immediately).
#' @param session_quiet Logical; suppress informational messages from the
#'   transport layer (CLI startup, handshake). Takes effect on next session
#'   init (call `hal_reset()` to apply immediately).
#' @param init_quiet Logical; when TRUE, suppress the data transmission notice
#'   shown on first session init (default: FALSE).
#' @param show_thoughts Logical; when TRUE, display the model's reasoning/thought
#'   process before the response (default: FALSE). Takes effect on next session
#'   init (call `hal_reset()` to apply immediately).
#' @param backend Character; transport backend: `"vscode"` (default in
#'   Positron) talks to the hal-bridge extension over localhost HTTP fronting
#'   `vscode.lm`; `"copilot"` (default elsewhere) uses the GitHub Copilot CLI in
#'   ACP mode; `"claude"` uses Anthropic's Claude Code CLI via `claude -p` with
#'   session resume. Takes effect on next session init (call `hal_reset()` to
#'   apply immediately).
#' @param quiet Logical; suppress confirmation messages (default: FALSE).
#'
#' @return Invisibly returns a list of changes made.
#'
#' @examples
#' hal_configure(stream = FALSE, do_retries = 1, quiet = TRUE)
#' hal_config()$stream
#' # unset again to restore the defaults
#' options(hal.stream = NULL, hal.do_retries = NULL)
#' @export
hal_configure <- function(
    use_colors        = NULL,
    stream            = NULL,
    stream_speed      = NULL,
    default_model     = NULL,
    system_prompt     = NULL,
    eval_denylist     = NULL,
    credential_action = NULL,
    eval_timeout      = NULL,
    prompt_timeout       = NULL,
    prompt_timeout_cold  = NULL,
    edit_in_place     = NULL,
    do_retries        = NULL,
    do_on_fail        = NULL,
    verify            = NULL,
    plot_vision       = NULL,
    permission_policy = NULL,
    session_quiet     = NULL,
    init_quiet        = NULL,
    show_thoughts     = NULL,
    backend           = NULL,
    quiet             = FALSE
) {
  changes <- list()

  if (!is.null(backend)) {
    valid <- c("vscode", "copilot", "claude")
    if (!backend %in% valid) {
      cli::cli_abort("{.arg backend} must be one of: {.val {valid}}")
    }
    options(hal.backend = backend)
    changes$backend <- paste("Set to", backend)
  }

  # Display options
  if (!is.null(use_colors)) {
    options(hal.use_colors = isTRUE(use_colors))
    changes$colors <- if (use_colors) "Enabled" else "Disabled"
  }

  if (!is.null(stream)) {
    options(hal.stream = isTRUE(stream))
    changes$streaming <- if (stream) "Enabled" else "Disabled"
  }

  if (!is.null(stream_speed)) {
    valid <- c("instant", "fast", "medium", "slow")
    if (!stream_speed %in% valid) {
      cli::cli_abort("stream_speed must be one of: {.val {valid}}")
    }
    options(hal.stream_speed = stream_speed)
    changes$speed <- paste("Set to", stream_speed)
  }

  if (!is.null(default_model)) {
    if (!is.character(default_model) || length(default_model) != 1L || !nzchar(default_model)) {
      cli::cli_abort("{.arg default_model} must be a single non-empty string.")
    }
    options(hal.default_model = default_model)
    changes$model <- paste("Set to", default_model)
  }

  # System prompt / persona
  if (!is.null(system_prompt)) {
    user <- .hal_get_current_user()
    system_prompt <- gsub("\\{user\\}", user, system_prompt)
    options(hal.system_prompt = system_prompt)
    changes$system_prompt <- "Updated"
  }

  # Governance options
  if (!is.null(eval_denylist)) {
    if (identical(eval_denylist, FALSE)) {
      options(hal.eval_denylist = FALSE)
      changes$eval_denylist <- "Disabled (full access)"
    } else {
      if (!is.character(eval_denylist)) {
        cli::cli_abort("{.arg eval_denylist} must be a character vector of function names.")
      }
      options(hal.eval_denylist = eval_denylist)
      changes$eval_denylist <- paste(length(eval_denylist), "blocked functions")
    }
  }

  if (!is.null(credential_action)) {
    valid <- c("warn", "redact", "block")
    if (!credential_action %in% valid) {
      cli::cli_abort("credential_action must be one of: {.val {valid}}")
    }
    options(hal.credential_action = credential_action)
    changes$credential_action <- paste("Set to", credential_action)
  }

  if (!is.null(eval_timeout)) {
    eval_timeout <- suppressWarnings(as.numeric(eval_timeout))
    if (is.na(eval_timeout) || eval_timeout < 0) {
      cli::cli_abort("eval_timeout must be a non-negative number.")
    }
    options(hal.eval_timeout = eval_timeout)
    changes$eval_timeout <- paste(eval_timeout, "seconds")
  }

  if (!is.null(prompt_timeout)) {
    prompt_timeout <- suppressWarnings(as.numeric(prompt_timeout))
    if (is.na(prompt_timeout) || prompt_timeout <= 0) {
      cli::cli_abort("prompt_timeout must be a positive number.")
    }
    options(hal.prompt_timeout = prompt_timeout)
    changes$prompt_timeout <- paste(prompt_timeout, "seconds")
  }

  if (!is.null(prompt_timeout_cold)) {
    prompt_timeout_cold <- suppressWarnings(as.numeric(prompt_timeout_cold))
    if (is.na(prompt_timeout_cold) || prompt_timeout_cold <= 0) {
      cli::cli_abort("prompt_timeout_cold must be a positive number.")
    }
    options(hal.prompt_timeout_cold = prompt_timeout_cold)
    changes$prompt_timeout_cold <- paste(prompt_timeout_cold, "seconds")
  }

  if (!is.null(edit_in_place)) {
    options(hal.edit_in_place = isTRUE(edit_in_place))
    changes$edit_in_place <- if (edit_in_place) "Enabled" else "Disabled"
  }

  if (!is.null(do_retries)) {
    do_retries <- suppressWarnings(as.integer(do_retries))
    if (is.na(do_retries) || do_retries < 0L) {
      cli::cli_abort("do_retries must be a non-negative integer.")
    }
    options(hal.do_retries = do_retries)
    changes$do_retries <- if (do_retries == 0L) {
      "Disabled"
    } else {
      paste(do_retries, "retries")
    }
  }

  if (!is.null(do_on_fail)) {
    valid <- c("warn", "abort")
    if (!is.character(do_on_fail) || length(do_on_fail) != 1L ||
        !do_on_fail %in% valid) {
      cli::cli_abort("{.arg do_on_fail} must be one of: {.val {valid}}")
    }
    options(hal.do_on_fail = do_on_fail)
    changes$do_on_fail <- paste("Set to", do_on_fail)
  }

  if (!is.null(verify)) {
    options(hal.verify = isTRUE(verify))
    changes$verify <- if (isTRUE(verify)) "Enabled" else "Disabled"
  }

  if (!is.null(plot_vision)) {
    options(hal.plot_vision = isTRUE(plot_vision))
    changes$plot_vision <- if (isTRUE(plot_vision)) "Enabled" else "Disabled"
  }

  if (!is.null(permission_policy)) {
    valid_str <- c("auto-allow", "auto-deny", "ask")
    if (!is.function(permission_policy) && !permission_policy %in% valid_str) {
      cli::cli_abort(
        "permission_policy must be {.val {valid_str}} or a function."
      )
    }
    options(hal.permission_policy = permission_policy)
    if (is.function(permission_policy)) {
      changes$permission_policy <- "Custom function"
    } else {
      changes$permission_policy <- paste("Set to", permission_policy)
    }
  }

  if (!is.null(session_quiet)) {
    options(hal.session_quiet = isTRUE(session_quiet))
    changes$session_quiet <- if (session_quiet) "Quiet" else "Verbose"
  }

  if (!is.null(init_quiet)) {
    options(hal.init_quiet = isTRUE(init_quiet))
    changes$init_quiet <- if (init_quiet) "Suppressed" else "Shown"
  }

  if (!is.null(show_thoughts)) {
    options(hal.show_thoughts = isTRUE(show_thoughts))
    changes$show_thoughts <- if (show_thoughts) "Enabled" else "Disabled"
  }

  if (!quiet && length(changes)) {
    cli::cli_alert_success("hal configuration updated.")
    for (nm in names(changes)) {
      cli::cli_bullets(setNames(paste0(tools::toTitleCase(nm), ": ", changes[[nm]]), "*"))
    }
  }

  invisible(changes)
}

#' Get Current hal Configuration
#'
#' Returns a named list of all active settings.
#'
#' @return A named list of configuration values.
#'
#' @examples
#' cfg <- hal_config()
#' cfg$backend
#' cfg$eval_timeout
#' @export
hal_config <- function() {
  list(
    # Display
    use_colors        = getOption("hal.use_colors", TRUE),
    stream            = getOption("hal.stream", TRUE),
    stream_speed      = getOption("hal.stream_speed", "medium"),
    # Connection
    backend           = .hal_backend(),
    default_model     = getOption("hal.default_model", NULL),
    # Governance
    eval_denylist     = getOption("hal.eval_denylist",
                                 .HAL_DEFAULT_EVAL_DENYLIST),
    credential_action = getOption("hal.credential_action", "warn"),
    eval_timeout      = getOption("hal.eval_timeout", 30),
    prompt_timeout       = getOption("hal.prompt_timeout", 60),
    prompt_timeout_cold  = getOption("hal.prompt_timeout_cold", 180),
    # Features
    # Unset = context-aware (TRUE in a saved IDE editor, FALSE at console);
    # report the "when applicable" default here.
    edit_in_place     = getOption("hal.edit_in_place", TRUE),
    do_retries        = getOption("hal.do_retries", 2L),
    # Unset = context-aware ("warn" interactive, "abort" non-interactive)
    do_on_fail        = getOption(
      "hal.do_on_fail", if (interactive()) "warn" else "abort"
    ),
    verify            = getOption("hal.verify", TRUE),
    plot_vision       = getOption("hal.plot_vision", TRUE),
    # Session
    permission_policy = getOption("hal.permission_policy", "auto-allow"),
    session_quiet     = getOption("hal.session_quiet", FALSE),
    init_quiet        = getOption("hal.init_quiet", FALSE),
    show_thoughts     = getOption("hal.show_thoughts", FALSE),
    # System prompt
    system_prompt     = getOption("hal.system_prompt", NULL),
    # User
    current_user      = .hal_get_current_user()
  )
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

#' Get current user login name
#' @keywords internal
#' @noRd
.hal_get_current_user <- function() {
  user <- Sys.getenv("USER", "")
  if (!nzchar(user)) user <- Sys.getenv("USERNAME", "")
  if (!nzchar(user)) user <- Sys.getenv("LOGNAME", "")
  if (!nzchar(user)) user <- "User"
  user
}
