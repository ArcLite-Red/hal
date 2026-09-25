# S3 classes for hal data objects:
#   hal_turn     -- one conversation turn
#   hal_response -- full prompt response (text + metadata)
#   hal_tool_call -- one tool invocation

# --- Turn -------------------------------------------------------------------

#' hal conversation turn
#'
#' An S3 class representing one turn in a hal conversation. Turns are
#' returned by [hal_history()] and tracked internally by [HalChat].
#'
#' @section Fields:
#' \describe{
#'   \item{`role`}{Character: `"user"`, `"assistant"`, or `"system"`.}
#'   \item{`content`}{Character: text content of the turn.}
#'   \item{`tool_calls`}{List of [hal_tool_call] objects, or `NULL`.}
#'   \item{`thoughts`}{Character vector of agent thought chunks, or `NULL`.}
#' }
#'
#' @name hal_turn
#' @return The `print()` method returns `x` invisibly. `hal_turn` objects
#'   themselves are created internally and returned by [hal_history()].
#' @seealso [hal_history()], [hal_response], [hal_tool_call]
NULL

#' Create a conversation turn
#'
#' @param role `"user"`, `"assistant"`, or `"system"`.
#' @param content Text content of the turn.
#' @param tool_calls List of `hal_tool_call` objects, or `NULL`.
#' @param thoughts Character vector of agent thought chunks, or `NULL`.
#' @return A `hal_turn` object.
#' @noRd
turn <- function(role, content = NULL, tool_calls = NULL, thoughts = NULL) {
  structure(
    compact(list(
      role = role,
      content = content,
      tool_calls = tool_calls,
      thoughts = thoughts
    )),
    class = "hal_turn"
  )
}

#' @rdname hal_turn
#' @param x A `hal_turn` object.
#' @param ... Ignored.
#' @export
print.hal_turn <- function(x, ...) {
  cat("<hal_turn>\n")
  cat("  role:", x$role, "\n")
  if (!is.null(x$content)) {
    text <- if (nchar(x$content) > 80) {
      paste0(substr(x$content, 1, 77), "...")
    } else {
      x$content
    }
    cat("  content:", text, "\n")
  }
  if (length(x$tool_calls) > 0) {
    cat("  tool_calls:", length(x$tool_calls), "\n")
    for (tc in x$tool_calls) {
      cat("    -", format(tc), "\n")
    }
  }
  if (length(x$thoughts) > 0) {
    cat("  thoughts:", length(x$thoughts), "chunks\n")
  }
  invisible(x)
}

# --- Response ----------------------------------------------------------------

#' hal prompt response
#'
#' An S3 class representing the full response from a single prompt. Returned
#' internally by [HalClient]'s `prompt()` method and used by [HalChat].
#'
#' @section Fields:
#' \describe{
#'   \item{`text`}{Character: the full response text.}
#'   \item{`stop_reason`}{Character: why the model stopped (e.g., `"end_turn"`, `"interrupted"`).}
#'   \item{`tool_calls`}{List of [hal_tool_call] objects.}
#'   \item{`thoughts`}{Character vector of agent thought text.}
#'   \item{`events`}{List of raw `session/update` events.}
#' }
#'
#' @name hal_response
#' @return The `print()` method returns `x` invisibly. `hal_response`
#'   objects are created internally by the transport clients.
#' @seealso [hal_turn], [hal_tool_call]
NULL

#' Create a prompt response
#'
#' @param text The full response text.
#' @param stop_reason Why the model stopped (e.g., `"end_turn"`).
#' @param tool_calls List of `hal_tool_call` objects.
#' @param thoughts Character vector of agent thought text.
#' @param events Raw session/update events (for advanced use).
#' @return A `hal_response` object.
#' @noRd
hal_response <- function(text = "", stop_reason = NULL, tool_calls = list(),
                             thoughts = character(), events = list()) {
  structure(
    list(
      text = text,
      stop_reason = stop_reason,
      tool_calls = tool_calls,
      thoughts = thoughts,
      events = events
    ),
    class = "hal_response"
  )
}

#' @rdname hal_response
#' @param x A `hal_response` object.
#' @param ... Ignored.
#' @export
print.hal_response <- function(x, ...) {
  cat("<hal_response>\n")
  if (nchar(x$text) > 0) {
    text <- if (nchar(x$text) > 200) {
      paste0(substr(x$text, 1, 197), "...")
    } else {
      x$text
    }
    cat("  text:", text, "\n")
  }
  if (!is.null(x$stop_reason)) {
    cat("  stop_reason:", x$stop_reason, "\n")
  }
  if (length(x$tool_calls) > 0) {
    cat("  tool_calls:", length(x$tool_calls), "\n")
    for (tc in x$tool_calls) {
      cat("    -", format(tc), "\n")
    }
  }
  if (length(x$thoughts) > 0) {
    cat("  thoughts:", length(x$thoughts), "chunks\n")
  }
  invisible(x)
}

# --- Tool Call ---------------------------------------------------------------

#' hal tool call
#'
#' An S3 class representing a single tool invocation by the agent. Tool calls
#' appear inside [hal_turn] objects and are visible when printing conversation
#' history.
#'
#' @section Fields:
#' \describe{
#'   \item{`tool_call_id`}{Character: unique ID for this tool call.}
#'   \item{`title`}{Character: human-readable description (e.g., "Viewing DESCRIPTION").}
#'   \item{`kind`}{Character: tool kind (`"read"`, `"write"`, `"command"`, etc.).}
#'   \item{`status`}{Character: `"pending"`, `"completed"`, or `"failed"`.}
#'   \item{`input`}{Named list of input arguments.}
#'   \item{`output`}{Character: output text from the tool.}
#' }
#'
#' @name hal_tool_call
#' @return The `print()` method returns `x` invisibly; the `format()`
#'   method returns a character scalar. `hal_tool_call` objects are
#'   created internally as tool activity streams in.
#' @seealso [hal_turn], [hal_response]
NULL

#' Create a tool call record
#'
#' @param tool_call_id Unique ID for this tool call.
#' @param title Human-readable description (e.g., "Viewing DESCRIPTION").
#' @param kind Tool kind (`"read"`, `"write"`, `"command"`, etc.).
#' @param status `"pending"`, `"completed"`, or `"failed"`.
#' @param input Named list of input arguments (from `rawInput`).
#' @param output Character string of output (from `rawOutput$content`).
#' @return A `hal_tool_call` object.
#' @noRd
hal_tool_call <- function(tool_call_id = NULL, title = NULL, kind = NULL,
                              status = "pending", input = NULL,
                              output = NULL) {
  structure(
    compact(list(
      tool_call_id = tool_call_id,
      title = title,
      kind = kind,
      status = status,
      input = input,
      output = output
    )),
    class = "hal_tool_call"
  )
}

#' @rdname hal_tool_call
#' @param x A `hal_tool_call` object.
#' @param ... Ignored.
#' @export
format.hal_tool_call <- function(x, ...) {
  # Use ASCII fallback on Windows to avoid cp1252 encoding errors
  use_ascii <- .Platform$OS.type == "windows" &&
    !identical(Sys.getenv("COPILOT_USE_UNICODE"), "true")
  icon <- if (use_ascii) {
    switch(x$status %||% "unknown",
      completed = "[OK]",
      failed = "[FAIL]",
      pending = "[...]",
      "[*]"
    )
  } else {
    switch(x$status %||% "unknown",
      completed = "\u2713",
      failed = "\u2717",
      pending = "\u2026",
      "\u2022"
    )
  }
  paste0(icon, " [", x$kind %||% "tool", "] ", x$title %||% x$tool_call_id)
}

#' @rdname hal_tool_call
#' @export
print.hal_tool_call <- function(x, ...) {
  cat("<hal_tool_call>\n")
  cat("  ", format(x), "\n")
  if (!is.null(x$input)) {
    cat("  input:", paste(names(x$input), x$input, sep = "=", collapse = ", "), "\n")
  }
  if (!is.null(x$output)) {
    out <- if (nchar(x$output) > 100) {
      paste0(substr(x$output, 1, 97), "...")
    } else {
      x$output
    }
    cat("  output:", out, "\n")
  }
  invisible(x)
}
