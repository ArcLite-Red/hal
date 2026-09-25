# ==============================================================================
# Backend dispatch — routes HalChat and friends to the configured transport
# ==============================================================================
#
# hal supports three backends behind a single surface:
#   vscode  — hal-bridge extension talking to vscode.lm (default in Positron)
#   copilot — GitHub Copilot CLI in ACP mode (default elsewhere; HalClient)
#   claude  — Anthropic Claude Code CLI via claude -p / --resume (HalClientClaude)
#
# Users select via `hal_configure(backend = "...")` or set
# `options(hal.backend = "...")` directly. When unset, the default is
# resolved at call time -- vscode if running inside Positron, copilot
# otherwise -- so existing Copilot-CLI users keep their setup while
# Positron users get the zero-CLI path automatically.

#' Resolve the active backend
#'
#' Honors an explicit argument or the `hal.backend` option first. If neither
#' is set, picks a sensible default: `vscode` when inside Positron (the
#' hal-bridge extension makes this the lowest-friction path), `copilot`
#' otherwise (preserves behaviour for existing users on RStudio / VS Code /
#' command-line R).
#'
#' @return `"vscode"`, `"copilot"`, or `"claude"`.
#' @keywords internal
#' @noRd
.hal_backend <- function(backend = NULL) {
  b <- backend %||% getOption("hal.backend", NULL)
  if (is.null(b) || identical(b, "")) {
    b <- if (.is_positron()) "vscode" else "copilot"
  }
  if (!b %in% c("copilot", "claude", "vscode")) {
    cli::cli_abort(c(
      "Unknown hal backend: {.val {b}}.",
      "i" = "Valid backends: {.val copilot}, {.val claude}, {.val vscode}."
    ))
  }
  b
}

#' Resolve the default model for a backend
#'
#' Per-backend defaults — free/cheap tier for each transport.
#' Explicit `model` or `hal.default_model` option takes precedence.
#' @keywords internal
#' @noRd
.hal_default_model <- function(backend = NULL) {
  b <- .hal_backend(backend)
  switch(b,
    # Sonnet on every backend. claude uses the never-stale alias; copilot/vscode
    # need a concrete id from their live model list, and those ids move as
    # GitHub/Microsoft retire older ones -- "claude-sonnet-4.6" was dropped from
    # both catalogs, so re-check these against hal_models() each release.
    # Note: vscode Sonnet burns premium quota (unlike "auto").
    copilot = "claude-sonnet-5",
    claude  = "sonnet",  # alias -> latest Sonnet; never goes stale (CLI resolves it)
    vscode  = "claude-sonnet-5"
  )
}

#' Build a transport client for the configured backend
#'
#' Factory used by `HalChat`, pipe verbs, and `hal_models()` so that the
#' session singleton and one-shot callers don't need to know which R6 class
#' to instantiate. Forwards all `...` args to the underlying client.
#'
#' @param backend Backend id, or `NULL` to read `hal.backend`.
#' @param ... Passed to `HalClient$new()` or `HalClientClaude$new()`.
#' @return An R6 client object implementing the shared public surface.
#' @keywords internal
#' @noRd
.hal_make_client <- function(backend = NULL, ...) {
  b <- .hal_backend(backend)
  args <- list(...)

  if (is.null(args$model) || identical(args$model, NA_character_)) {
    args$model <- .hal_default_model(b)
  }

  switch(b,
    copilot = do.call(HalClient$new, args),
    claude  = do.call(HalClientClaude$new, args),
    vscode  = do.call(HalClientVSCode$new, args)
  )
}
