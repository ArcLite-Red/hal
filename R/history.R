# ==============================================================================
# hal History -- Conversation history viewing and search
# ==============================================================================

#' View Conversation History
#'
#' Display or return the conversation turns from the active session.
#'
#' @param role Filter by role: `"user"`, `"assistant"`, `"system"`, or
#'   `NULL` for all.
#' @param pattern Optional regex to filter turn content.
#' @param format Output format: `"console"` (print to screen), `"data.frame"`,
#'   or `"text"` (character vector).
#' @param n Maximum number of turns to return (most recent). `NULL` for all.
#'
#' @return Depends on `format`: invisible NULL for console, a data.frame, or
#'   a character vector.
#'
#' @examples
#' # with no active session this returns an empty data frame
#' hal_history(format = "data.frame")
#' \dontrun{
#' hal_history()                      # print all turns
#' hal_history(role = "user", n = 5)  # last five user prompts
#' hal_history(pattern = "ggplot")    # turns mentioning ggplot
#' }
#' @export
hal_history <- function(role = NULL, pattern = NULL,
                            format = c("console", "data.frame", "text"),
                            n = NULL) {
  format <- match.arg(format)
  session <- .hal_get_session()

  if (is.null(session$chat)) {
    cli::cli_alert_info("No active session.")
    if (format == "data.frame") return(data.frame())
    if (format == "text") return(character())
    return(invisible(NULL))
  }

  turns <- session$chat$get_turns(include_system_prompt = TRUE)
  if (!length(turns)) {
    cli::cli_alert_info("No conversation turns.")
    if (format == "data.frame") return(data.frame())
    if (format == "text") return(character())
    return(invisible(NULL))
  }

  # Filter by role
  if (!is.null(role)) {
    turns <- Filter(function(t) t$role == role, turns)
  }

  # Filter by pattern
  if (!is.null(pattern)) {
    turns <- Filter(function(t) {
      !is.null(t$content) && grepl(pattern, t$content, ignore.case = TRUE)
    }, turns)
  }

  # Tail to n

  if (!is.null(n) && length(turns) > n) {
    turns <- turns[seq(length(turns) - n + 1, length(turns))]
  }

  # Format output
  switch(format,
    "console" = {
      for (t in turns) {
        role_label <- toupper(t$role)
        text <- t$content %||% ""
        if (nchar(text) > 200) text <- paste0(substr(text, 1, 197), "...")
        cli::cli_text("{.strong [{role_label}]} {text}")
      }
      invisible(NULL)
    },
    "data.frame" = {
      data.frame(
        role = vapply(turns, function(t) t$role, character(1)),
        content = vapply(turns, function(t) t$content %||% "", character(1)),
        n_tool_calls = vapply(turns, function(t) length(t$tool_calls), integer(1)),
        stringsAsFactors = FALSE
      )
    },
    "text" = {
      vapply(turns, function(t) {
        paste0("[", t$role, "] ", t$content %||% "")
      }, character(1))
    }
  )
}
