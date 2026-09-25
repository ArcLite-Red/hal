# ==============================================================================
# hal_usage() -- Session-level cost visibility
# ==============================================================================
#
# Every hal() turn is logged with the model used at that moment. hal_usage()
# tallies the log and joins against the model tier catalog (0x, 0.33x, 1x, 3x)
# to turn opaque quota usage into budgetable units.
#
# Tracking is in-memory only (session$usage_log). Cleared by hal_reset().

#' Session usage summary
#'
#' Tallies `hal()` turns by model and, when tier info is available, computes
#' cumulative Copilot billing units. Usage is tracked per session in memory
#' and cleared by [hal_reset()].
#'
#' @param tiers Optional named character vector mapping `modelId` to usage
#'   tier (e.g. `c("claude-sonnet-5" = "1x")`). When `NULL` (default), uses
#'   the cached tier map populated from the last [hal_models()] call in this
#'   session, or fetches it if none is cached and a live CLI is available.
#'   Pass a pre-computed map to skip the network hop.
#'
#' @return A `hal_usage` data frame with columns `model`, `turns`, `tier`,
#'   `units` (numeric tier × turns, `NA` if tier unknown). Prints a compact
#'   summary including the session total.
#'
#' @examples
#' \dontrun{
#' hal("hello")
#' hal("follow-up", model = "gpt-4.1")
#' hal_usage()
#' }
#'
#' @export
hal_usage <- function(tiers = NULL) {
  session <- .hal_get_session()
  log <- session$usage_log %||% list()

  if (length(log) == 0L) {
    out <- data.frame(
      model = character(),
      turns = integer(),
      tier = character(),
      units = numeric(),
      stringsAsFactors = FALSE
    )
    class(out) <- c("hal_usage", "data.frame")
    attr(out, "total_turns") <- 0L
    attr(out, "total_units") <- 0
    return(out)
  }

  models <- vapply(log, function(r) r$model %||% NA_character_, character(1))
  counts <- table(models, useNA = "ifany")
  model_names <- names(counts)

  # Resolve tiers
  tiers <- tiers %||% session$model_tiers %||% .hal_fetch_tiers()
  if (!is.null(tiers)) session$model_tiers <- tiers

  tier_chr <- unname(tiers[model_names])
  tier_chr[is.na(tier_chr)] <- NA_character_

  out <- data.frame(
    model = ifelse(is.na(model_names), "(default)", model_names),
    turns = as.integer(counts),
    tier = tier_chr,
    units = .hal_tier_numeric(tier_chr) * as.integer(counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  out <- out[order(-out$turns), , drop = FALSE]
  row.names(out) <- NULL

  class(out) <- c("hal_usage", "data.frame")
  attr(out, "total_turns") <- sum(out$turns)
  attr(out, "total_units") <- sum(out$units, na.rm = TRUE)
  out
}

#' @export
print.hal_usage <- function(x, ...) {
  total_turns <- attr(x, "total_turns") %||% 0L
  total_units <- attr(x, "total_units") %||% 0

  cli::cli_rule("hal session usage")
  if (nrow(x) == 0L) {
    cli::cli_alert_info("No turns recorded this session.")
    return(invisible(x))
  }

  # Plain data frame view (drop attrs for the tabular print)
  df <- as.data.frame(unclass(x))
  df$units <- ifelse(is.na(df$units), "-", format(df$units, nsmall = 2))
  df$tier  <- ifelse(is.na(df$tier),  "?", df$tier)
  print(df, row.names = FALSE)

  cat(sprintf(
    "\n  Total: %d turn%s, %s units\n\n",
    total_turns,
    if (total_turns == 1L) "" else "s",
    if (total_units == 0 && any(is.na(x$tier))) "~" else format(total_units, nsmall = 2)
  ))
  invisible(x)
}

#' Record a completed hal() turn against the current model
#'
#' Appends to `session$usage_log`. Called from [hal()] after a successful
#' response.
#'
#' @keywords internal
#' @noRd
.hal_record_turn <- function(model = NULL) {
  session <- .hal_get_session()
  entry <- list(
    model = model %||% session$model,
    timestamp = Sys.time()
  )
  session$usage_log <- c(session$usage_log %||% list(), list(entry))
  invisible(entry)
}

#' Fetch model tier map from the live CLI, returning NULL on failure
#' @keywords internal
#' @noRd
.hal_fetch_tiers <- function() {
  tryCatch({
    df <- hal_models()
    if (!is.data.frame(df) || nrow(df) == 0L) return(NULL)
    setNames(as.character(df$usage), df$id)
  }, error = function(e) NULL)
}

#' Convert a tier string like "1x" / "0.33x" / "3x" to a numeric multiplier
#' @keywords internal
#' @noRd
.hal_tier_numeric <- function(tier) {
  vapply(tier, function(t) {
    if (is.na(t) || !nzchar(t)) return(NA_real_)
    n <- suppressWarnings(as.numeric(sub("x$", "", t)))
    if (is.na(n)) NA_real_ else n
  }, numeric(1), USE.NAMES = FALSE)
}
