# ==============================================================================
# hal_quota() -- Claude 5-hour window visibility
# ==============================================================================
#
# Claude Code subscription auth carries a 5-hour rolling window quota. Each
# prompt stream emits at least one `rate_limit_event` with a `rate_limit_info`
# payload (rateLimitType, status, resetsAt, overageStatus,
# overageDisabledReason). The Claude backend stashes the latest payload on
# the client; hal_quota() surfaces it as a structured object.
#
# Copilot backend returns NULL with an informational message -- flat-rate
# subscription, no equivalent surface.

#' Claude 5-hour window quota status
#'
#' Returns the most recent `rate_limit_event` payload observed on the Claude
#' backend during this session. Only populated after at least one `hal()`,
#' `hal_ask()`, or `hal_do()` call -- the event is embedded in the prompt
#' response stream.
#'
#' @return A `hal_quota` object (list) with fields `type`, `status`,
#'   `resets_at` (POSIXct), `overage_status`, `overage_disabled_reason`,
#'   `backend`. Returns `NULL` invisibly if no rate-limit info has been
#'   observed, or if the active backend is Copilot.
#'
#' @examples
#' \dontrun{
#' hal("hello")
#' hal_quota()
#' }
#'
#' @export
hal_quota <- function() {
  backend <- .hal_backend()
  if (!identical(backend, "claude")) {
    cli::cli_alert_info(
      "Quota tracking is only available on the Claude backend."
    )
    return(invisible(NULL))
  }

  session <- .hal_get_session()
  chat <- session$chat
  if (is.null(chat)) {
    cli::cli_alert_info("No active session. Call {.code hal()} first.")
    return(invisible(NULL))
  }

  client <- chat$get_client()
  info <- tryCatch(client$get_rate_limit(), error = function(e) NULL)
  if (is.null(info)) {
    cli::cli_alert_info(
      "No rate-limit info observed yet. Call {.code hal()} first."
    )
    return(invisible(NULL))
  }

  out <- list(
    type = info$rateLimitType %||% NA_character_,
    status = info$status %||% NA_character_,
    resets_at = .hal_quota_posix(info$resetsAt),
    overage_status = info$overageStatus %||% NA_character_,
    overage_disabled_reason = info$overageDisabledReason %||% NA_character_,
    backend = backend
  )
  class(out) <- c("hal_quota", "list")
  out
}

#' @export
print.hal_quota <- function(x, ...) {
  cli::cli_rule("hal quota ({x$backend})")
  status_icon <- if (identical(x$status, "allowed")) "v" else "!"
  cli::cli_inform(c(
    "i" = "Window: {.val {x$type}}",
    setNames("Status: {.val {x$status}}", status_icon),
    "i" = "Resets: {format(x$resets_at, '%Y-%m-%d %H:%M:%S %Z')}",
    "i" = "Overage: {.val {x$overage_status}}"
  ))
  if (!is.na(x$overage_disabled_reason) && nzchar(x$overage_disabled_reason)) {
    cli::cli_alert_warning(
      "Overage disabled: {.val {x$overage_disabled_reason}}"
    )
  }
  invisible(x)
}

#' Convert a `rate_limit_info$resetsAt` value to POSIXct, tolerant of shape.
#' @keywords internal
#' @noRd
.hal_quota_posix <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA))
  # Unix epoch seconds (number or numeric-string)
  n <- suppressWarnings(as.numeric(x))
  if (!is.na(n)) return(as.POSIXct(n, origin = "1970-01-01"))
  # Fallback: ISO-8601 string
  parsed <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (inherits(parsed, "POSIXct") && !is.na(parsed)) return(parsed)
  as.POSIXct(NA)
}
