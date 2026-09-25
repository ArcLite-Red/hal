# ==============================================================================
# hal_plan() -- Locate the SDK's plan.md for the current session
# ==============================================================================
#
# In Plan mode (and occasionally Agent mode), the Copilot SDK writes a
# structured plan to `~/.copilot/session-state/{sessionId}/plan.md`. This
# function surfaces the path so users can view, edit, or read it without
# hunting through hidden directories.

#' Locate the current session's plan.md
#'
#' Plan mode (and occasionally Agent mode) asks the model to commit to a
#' structured plan, written to
#' `~/.copilot/session-state/{sessionId}/plan.md`. `hal_plan()` returns
#' the path so you can view or edit it directly.
#'
#' @return Character path to plan.md, or `NULL` invisibly if no plan is
#'   available.
#'
#' @examples
#' \dontrun{
#' hal_reset(mode = "plan")
#' hal("Investigate the Q4 revenue drop in `sales`")
#' hal_plan()                       # path to plan.md
#' file.edit(hal_plan())            # open in editor
#' readLines(hal_plan())            # read programmatically
#' }
#'
#' @export
hal_plan <- function() {
  session <- .hal_get_session()
  if (is.null(session$chat)) {
    cli::cli_alert_info(
      "No active hal session. Call {.code hal()} or {.code hal_reset()} first."
    )
    return(invisible(NULL))
  }

  sid <- tryCatch(
    session$chat$get_client()$get_session_id(),
    error = function(e) NULL
  )
  if (is.null(sid) || !nzchar(sid)) {
    cli::cli_alert_info(
      "Session has not yet been created on the server. Send a prompt first."
    )
    return(invisible(NULL))
  }

  plan_file <- .hal_plan_source_path(sid)
  if (!file.exists(plan_file)) {
    cli::cli_alert_info(c(
      "No {.file plan.md} written for this session yet.",
      "i" = "Plans appear after the model commits to one (typically in Plan mode).",
      "i" = "Path: {.path {plan_file}}"
    ))
    return(invisible(NULL))
  }

  plan_file
}

#' Resolve the SDK's plan.md path for a session ID
#' @keywords internal
#' @noRd
.hal_plan_source_path <- function(session_id) {
  root <- getOption("hal.plan_root", "~/.copilot/session-state")
  file.path(path.expand(root), session_id, "plan.md")
}
