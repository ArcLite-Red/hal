# ==============================================================================
# hal eval_r -- Core execution tool
# ==============================================================================
#
# eval_r: Executes R code in the user's LIVE R session (caller env).
# Always registered as an MCP tool at session init.
# Governance controls (denylist, credential scanner, timeout) are enforced.

# ------------------------------------------------------------------------------
# eval_r tool
# ------------------------------------------------------------------------------

#' Execute R code in the caller's environment (tool function)
#'
#' @param code Character string of R code to execute.
#' @return Character string with output, structure, and assignment info.
#' @keywords internal
#' @noRd
.hal_eval_r <- function(code) {
  # Guard against the common malformed call where the model invokes eval_r
  # with no arguments. Returning a directive error message lets the model
  # retry with the right shape rather than getting R's raw "argument missing".
  if (missing(code) || is.null(code) || !is.character(code) ||
      length(code) != 1L || !nzchar(trimws(code))) {
    return(paste(
      "Error: eval_r requires a non-empty `code` argument.",
      'Call shape: {"code": "<R expression>"}.',
      'Example: {"code": "ls()"} or {"code": "head(mtcars)"}.'
    ))
  }

  session <- .hal_get_session()
  eval_env <- session$eval_caller_env %||% globalenv()

  # Governance: denylist check
  blocked_msg <- .hal_check_eval_denylist(code)
  if (!is.null(blocked_msg)) return(blocked_msg)

  tryCatch({
    # Plot vision: snapshot device state before eval (NULL when disabled)
    plot_snap <- if (.hal_plot_vision_enabled()) .hal_plot_snapshot() else NULL

    expr <- parse(text = code)

    # Snapshot env names before eval
    names_before <- ls(eval_env)

    output <- utils::capture.output({
      val <- .hal_eval_with_timeout(expr, eval_env)
    })

    # Detect new assignments
    names_after <- ls(eval_env)
    new_names <- setdiff(names_after, names_before)

    if (length(new_names)) {
      session$eval_r_assigned <- unique(c(session$eval_r_assigned, new_names))
    }

    # Plot vision: render returned plot objects, diff the device, capture
    cap <- if (!is.null(plot_snap)) {
      .hal_plot_capture(plot_snap, val)
    } else {
      list(image = NULL, render_error = NULL)
    }

    parts <- character()
    if (length(output)) {
      parts <- c(parts, "## Output:", paste(output, collapse = "\n"))
    }

    val_str <- tryCatch(
      paste(utils::capture.output(utils::str(val)), collapse = "\n"),
      error = function(e) NULL
    )
    if (!is.null(val_str)) parts <- c(parts, "## Structure:", val_str)

    if (length(new_names)) {
      parts <- c(parts, paste0(
        "## Assigned to user environment: ",
        paste(new_names, collapse = ", ")
      ))
    }

    if (!is.null(cap$image)) {
      parts <- c(parts, "## Plot: rendered and attached as an image.")
    } else if (!is.null(cap$render_error)) {
      parts <- c(parts, paste0("## Plot render error: ", cap$render_error))
    }

    result <- paste(parts, collapse = "\n\n")
    if (nchar(result) > 8000L) {
      result <- paste0(substr(result, 1, 8000L), "\n...[truncated]")
    }
    if (!nzchar(result)) result <- "(no visible output)"

    # Governance: scan output for credentials (text only -- the image
    # base64 never passes through the scanner or the truncation above)
    result <- .hal_scan_outbound(result)

    if (is.null(cap$image)) return(result)

    if (!isTRUE(getOption("hal.session_quiet", FALSE))) {
      cli::cli_alert_info("hal: plot captured for the model.")
    }
    structure(list(text = result, image = cap$image),
              class = "hal_tool_result")
  }, error = function(e) paste("Error:", conditionMessage(e)))
}

#' Build eval_r as a hal tool definition
#'
#' Returns a plain list suitable for `$register_tool()`.
#'
#' @return A tool definition list.
#' @keywords internal
#' @noRd
.hal_eval_r_tool_def <- function() {
  list(
    name = "eval_r",
    description = paste(
      "Execute R code in the user's live R session and return printed output",
      "plus str() of the result.",
      "REQUIRED ARGUMENT: `code` (string of R code).",
      "Example calls:",
      '`{"code": "mean(df$x)"}`,',
      '`{"code": "ls()"}`,',
      '`{"code": "head(mtcars, 3)"}`.',
      "Never call this tool without a `code` argument.",
      "Objects in the user's environment are accessible by name (e.g., df,",
      "my_model). Only this tool can see session objects -- prefer it over",
      "bash/Rscript. Use for computation, object inspection, or variable",
      "manipulation. Do not use for pure language tasks."
    ),
    fun = .hal_eval_r,
    parameters = list(
      type = "object",
      properties = list(
        code = list(
          type = "string",
          description = paste(
            "R code to execute. Required. Pass a complete R expression",
            "or statement, e.g. 'mean(df$x)' or 'summary(model)'.",
            "Do not pass an empty string."
          )
        )
      ),
      required = list("code"),
      additionalProperties = FALSE
    )
  )
}

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

#' Strip fenced code blocks
#' @keywords internal
#' @noRd
.hal_strip_fences <- function(txt) {
  if (length(txt) == 0L) return(character())
  lines <- if (length(txt) == 1L) strsplit(txt, "\n", fixed = TRUE)[[1]] else {
    unlist(txt, use.names = FALSE)
  }
  if (!length(lines)) return(character())

  open_idx <- which(grepl("^```(?:\\s*[rR])?\\s*$", lines))
  if (!length(open_idx)) {
    while (length(lines) && !nzchar(lines[1])) lines <- lines[-1]
    while (length(lines) && !nzchar(lines[length(lines)])) lines <- lines[-length(lines)]
    return(lines)
  }

  open <- open_idx[1]
  close_after <- which(grepl("^```\\s*$", lines))
  close_after <- close_after[close_after > open]
  if (!length(close_after)) {
    body <- if (open + 1L <= length(lines)) lines[(open + 1L):length(lines)] else character()
  } else {
    close <- close_after[1]
    body <- if (close > (open + 1L)) lines[(open + 1L):(close - 1L)] else character()
  }

  while (length(body) && !nzchar(body[1])) body <- body[-1]
  while (length(body) && !nzchar(body[length(body)])) body <- body[-length(body)]
  body
}

# ------------------------------------------------------------------------------
# IPC eval function -- runs in the user's live R session
# ------------------------------------------------------------------------------

#' Execute eval_r code via IPC in the user's live session
#'
#' This is the callback that HalClient calls when it detects an IPC eval
#' request from the MCP subprocess. It runs the code directly in the user's
#' caller environment.
#'
#' @param code Character string of R code to execute.
#' @return A list with `result` (character) and `error` (character or NULL).
#' @keywords internal
#' @noRd
.hal_ipc_eval_fn <- function(code) {
  # Mirror the local-call guard so MCP-routed callers also get a directive
  # error rather than R's raw "argument 'code' is missing" message.
  if (missing(code) || is.null(code) || !is.character(code) ||
      length(code) != 1L || !nzchar(trimws(code))) {
    return(list(
      result = paste(
        "Error: eval_r requires a non-empty `code` argument.",
        'Call shape: {"code": "<R expression>"}.',
        'Example: {"code": "ls()"} or {"code": "head(mtcars)"}.'
      ),
      error = NULL
    ))
  }

  session <- .hal_get_session()
  eval_env <- session$eval_caller_env %||% globalenv()

  # Governance: denylist check
  blocked <- .hal_check_eval_denylist(code)
  if (!is.null(blocked)) return(list(result = blocked, error = NULL))

  tryCatch({
    # Plot vision: snapshot device state before eval (NULL when disabled)
    plot_snap <- if (.hal_plot_vision_enabled()) .hal_plot_snapshot() else NULL

    expr <- parse(text = code)

    names_before <- ls(eval_env)

    output <- utils::capture.output({
      val <- .hal_eval_with_timeout(expr, eval_env)
    })

    names_after <- ls(eval_env)
    new_names <- setdiff(names_after, names_before)

    # Track assignments
    if (length(new_names)) {
      session$eval_r_assigned <- unique(c(
        session$eval_r_assigned %||% character(), new_names
      ))
    }

    # Plot vision: render returned plot objects, diff the device, capture
    cap <- if (!is.null(plot_snap)) {
      .hal_plot_capture(plot_snap, val)
    } else {
      list(image = NULL, render_error = NULL)
    }

    parts <- character()
    if (length(output)) {
      parts <- c(parts, "## Output:", paste(output, collapse = "\n"))
    }

    val_str <- tryCatch(
      paste(utils::capture.output(utils::str(val)), collapse = "\n"),
      error = function(e) NULL
    )
    if (!is.null(val_str)) parts <- c(parts, "## Structure:", val_str)

    if (length(new_names)) {
      parts <- c(parts, paste0(
        "## New variables: ", paste(new_names, collapse = ", ")
      ))
    }

    if (!is.null(cap$image)) {
      parts <- c(parts, "## Plot: rendered and attached as an image.")
    } else if (!is.null(cap$render_error)) {
      parts <- c(parts, paste0("## Plot render error: ", cap$render_error))
    }

    result_text <- paste(parts, collapse = "\n\n")
    if (nchar(result_text) > 8000L) {
      result_text <- paste0(substr(result_text, 1, 8000L), "\n...[truncated]")
    }
    if (!nzchar(result_text)) result_text <- "(no visible output)"

    # Governance: scan output for credentials (text only -- the image
    # base64 never passes through the scanner or the truncation above)
    result_text <- .hal_scan_outbound(result_text)

    if (!is.null(cap$image) &&
        !isTRUE(getOption("hal.session_quiet", FALSE))) {
      cli::cli_alert_info("hal: plot captured for the model.")
    }

    list(result = result_text, error = NULL, image = cap$image)
  }, error = function(e) {
    list(result = NULL, error = paste("Error:", conditionMessage(e)))
  })
}

# ------------------------------------------------------------------------------
# Tool registration helpers
# ------------------------------------------------------------------------------

#' Register eval_r tool on a HalChat session
#'
#' Also sets up IPC so eval_r executes in the user's live R session
#' (not the MCP subprocess).
#'
#' @param chat A HalChat instance.
#' @return Invisibly returns TRUE.
#' @keywords internal
#' @noRd
.hal_ensure_eval_tools <- function(chat) {
  # Idempotency: skip if already registered
  session <- .hal_get_session()
  if (isTRUE(session$eval_tools_registered)) return(invisible(TRUE))
  session$eval_tools_registered <- TRUE

  chat$register_tool(.hal_eval_r_tool_def())

  # Permission bridge: registered unconditionally because it's only invoked
  # when the Claude backend emits `--permission-prompt-tool` (i.e., when the
  # user supplies a function policy). Harmless on Copilot.
  chat$register_tool(.hal_permission_prompt_tool_def())

  # Self-help: lets the model fetch authoritative Rd content for hal topics
  # instead of guessing. Skipped silently if the bundle is missing.
  help_def <- .hal_help_tool_def()
  if (!is.null(help_def)) chat$register_tool(help_def)

  # Set up IPC for live eval_r + permission round-trips
  session <- .hal_get_session()
  if (is.null(session$ipc_dir)) {
    session$ipc_dir <- tempfile("hal_ipc_")
    # mode = "0700": the IPC dir carries eval-request code and results that the
    # parent session executes. Owner-only perms stop another local user from
    # dropping a request-*.json (code injection) or reading results. POSIX-only;
    # Windows ignores `mode` but %TEMP% is already per-user. Defense in depth on
    # top of R's per-session tempdir (itself 0700).
    dir.create(session$ipc_dir, recursive = TRUE, showWarnings = FALSE,
               mode = "0700")
  }

  client <- chat$get_client()
  client$set_ipc(session$ipc_dir, .hal_ipc_eval_fn, .hal_ipc_permission_fn)

  invisible(TRUE)
}
