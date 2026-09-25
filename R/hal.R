# ==============================================================================
# hal() -- Main conversational entry point
# ==============================================================================
#
# Uses the global singleton session for stateful conversation.
# String-only input, model-driven exploration, optional env access via eval_r.

#' Chat with GitHub Copilot
#'
#' The main entry point for conversational interaction with Copilot models.
#' Maintains a persistent session across calls. The SDK agent has built-in
#' tools for file operations, code search, and shell commands.
#'
#' @param ... Character strings, concatenated as the user message.
#' @param model Model identifier. If NULL, uses the session/configured default.
#' @param use_env Logical or NULL; if TRUE, snapshots the caller's environment
#'   and injects it into the prompt so the model naturally uses eval_r to work
#'   with your R objects. If NULL (default), auto-detects: scans the prompt
#'   for R identifiers and injects the snapshot only when the prompt mentions
#'   an object that exists in the caller's environment. When auto-detect
#'   fires, a `cli::cli_alert_info()` announces the matched names (suppress
#'   with `hal_configure(session_quiet = TRUE)`). Set FALSE to disable.
#' @param reset Logical; if TRUE, starts a fresh session before prompting.
#' @param inspect Logical; if TRUE, returns session info instead of prompting.
#'
#' @return The assistant's text response (invisibly when displayed).
#'
#' @section Errors in scripts:
#' In an interactive session a transport failure prints an alert and
#' returns `invisible(NULL)`. In non-interactive contexts (scripts, R
#' Markdown) it aborts with a classed condition instead, so programmatic
#' callers can handle hal failures specifically:
#' `tryCatch(hal("..."), hal_transport_error = function(e) ...)`. All hal
#' conditions inherit from `hal_error` / `hal_warning`; `hal_do()` signals
#' `hal_do_error` / `hal_do_warning`, and `hal_ask()` signals
#' `hal_ask_warning`.
#'
#' @examples
#' \dontrun{
#' # Simple conversation
#' hal("What is R?")
#' hal("Can you explain more about data frames?")
#'
#' # Environment access (auto-detected when objects exist)
#' df <- mtcars
#' hal("What's the mean mpg?")   # auto-detects df, injects env snapshot
#'
#' # Reset and start fresh
#' hal("Hello", reset = TRUE)
#'
#' # Inspect session
#' hal(inspect = TRUE)
#' }
#'
#' @export
hal <- function(..., model = NULL, use_env = NULL,
                reset = FALSE, inspect = FALSE) {

  # Inspect mode
 if (isTRUE(inspect)) {
    session <- .hal_get_session()
    info <- list(
      active = !is.null(session$chat),
      model = session$model,
      turns = if (!is.null(session$chat)) length(session$chat$get_turns()) else 0L,
      tools = session$tool_names %||% character(),
      spawn_count = session$spawn_count %||% 0L
    )
    return(info)
  }

  # Build prompt from dots
  dots <- list(...)
  if (!length(dots)) {
    cli::cli_abort("Provide at least one prompt string, or use {.code inspect = TRUE}.")
  }
  if (!all(vapply(dots, is.character, logical(1)))) {
    cli::cli_abort("All arguments to {.fn hal} must be character strings.")
  }
  prompt <- paste(unlist(dots), collapse = " ")

  # Reset if requested
  if (isTRUE(reset)) {
    .hal_initialize(model = model, force = TRUE)
  }

  # Ensure session
  session <- .hal_ensure_session()

  # Switch model if specified and different
  if (!is.null(model) && !identical(model, session$model)) {
    session$chat$switch_model(model)
    session$model <- model
  }

  # Stash caller env for eval_r
  session$eval_caller_env <- parent.frame()
  session$eval_r_assigned <- character()

  # Offer to create hal.md if absent (asks first; never writes unasked)
  .hal_ensure_memory_file()

  # Inject hal.md project memory on first turn of a session
  if (!isTRUE(session$memory_injected)) {
    memory_content <- .hal_read_memory_file()
    if (nzchar(memory_content)) {
      prompt <- paste0(
        prompt,
        "\n\n[Project memory from hal.md -- read for context, ",
        "update via edit tool when you learn something worth preserving:\n",
        memory_content, "\n]"
      )
    }
    session$memory_injected <- TRUE
  }

  # Inject environment context
  # NULL (default) = auto-detect: inject when the prompt references an
  #   object that exists in the caller's environment
  # TRUE = always inject, FALSE = never inject
  if (is.null(use_env)) {
    caller_env <- parent.frame()
    matched <- .hal_detect_env_refs(prompt, caller_env)
    use_env <- length(matched) > 0L
    if (use_env && !isTRUE(getOption("hal.session_quiet", FALSE))) {
      cli::cli_alert_info(
        "Injecting env context (matched: {.val {matched}})"
      )
    }
  }
  if (isTRUE(use_env)) {
    env_desc <- .hal_describe_env_layered(parent.frame())
    if (nzchar(env_desc)) {
      prompt <- paste0(
        prompt,
        "\n\n[Environment objects available via eval_r:\n", env_desc,
        "\nUse the eval_r tool (not bash/Rscript) to inspect or modify these objects.]"
      )
    }
  }

  # Governance: scan outbound prompt
  prompt <- .hal_scan_outbound(prompt)

  # Reset thought state for this turn
  session$thought_active <- FALSE

  # Send prompt (echo = "none" -- we handle display ourselves).
  # Interactive: pretty-print and return NULL (forgiving at the console).
  # Non-interactive: rethrow as a classed condition so scripts can
  # distinguish "transport died" from "model said nothing".
  response_text <- tryCatch(
    session$chat$chat(prompt),
    error = function(e) {
      if (!interactive()) {
        cli::cli_abort(
          "hal: request failed: {e$message}",
          class = c("hal_transport_error", "hal_error"),
          parent = e
        )
      }
      cli::cli_alert_danger("Error: {e$message}")
      NULL
    }
  )

  # Close thought display if it was active
  if (isTRUE(session$thought_active)) {
    .hal_thought_close()
    session$thought_active <- FALSE
  }

  if (is.null(response_text)) return(invisible(NULL))

  # Record turn for hal_usage()
  .hal_record_turn(model = session$model)

  # Display with formatting
  .hal_display_response(response_text)

  invisible(response_text)
}
