#' List available models
#'
#' Returns a data frame describing the models exposed by the active backend.
#'
#' - **Copilot**: queries the ACP server (`session/new`) to fetch live
#'   model metadata.
#' - **Claude**: returns the alias selectors (`opus`/`sonnet`/`haiku`), which
#'   Anthropic resolves to the latest model of each tier -- the Claude CLI
#'   exposes no live model list. Pass a concrete id (e.g. `"claude-opus-5"`)
#'   to `model=` if you need to pin a version.
#' - **vscode**: queries the hal-bridge `/models` endpoint, which returns
#'   whatever models `vscode.lm` exposes to your Copilot session.
#'
#' @param client A [HalClient] instance, or `NULL` to create one (Copilot only).
#' @return A data frame with columns:
#'   - `id` — model identifier (matches `--model` flag)
#'   - `name` — display name
#'   - `description` — vendor-supplied prose (may be `NA`)
#'   - `provider` — `"openai"`, `"anthropic"`, `"google"`, or `"unknown"`
#'   - `family` — coarse tier: `"light"`, `"standard"`, `"heavy"`
#'   - `usage` — billing tier label (e.g. `"1x"`, `"0.33x"`, `"0x"`)
#'   - `multiplier` — numeric form of `usage`
#'   - `is_default` — `TRUE` if currently selected as session default
#'   - `context_window` — token limit when known (`NA` otherwise)
#'   - `release_date` — ISO date when known (`NA` otherwise)
#'
#' @seealso [hal_model_info()] for full per-model detail (incl. raw `_meta`).
#'
#' @export
#' @examples
#' \dontrun{
#' hal_models()
#' subset(hal_models(), provider == "anthropic")
#' }
hal_models <- function(client = NULL) {
  if (is.null(client) && identical(.hal_backend(), "claude")) {
    return(.hal_claude_models())
  }

  if (is.null(client) && identical(.hal_backend(), "vscode")) {
    return(.hal_vscode_models())
  }

  own_client <- is.null(client)
  client <- client %||% HalClient$new(quiet = TRUE)
  on.exit(if (own_client) client$stop(), add = TRUE)

  session <- client$new_session()
  models <- session$models$availableModels %||% list()
  current_id <- session$models$currentModelId %||% NA_character_

  if (length(models) == 0) {
    stderr <- client$read_stderr()
    if (nzchar(stderr) && grepl("model", stderr, ignore.case = TRUE)) {
      cli::cli_inform(c(
        "!" = "No models returned from session.",
        "i" = "CLI stderr: {stderr}"
      ))
    } else {
      cli::cli_inform(c(
        "!" = "No models returned from session.",
        "i" = "The Copilot CLI may have failed to fetch models from the API.",
        "i" = "Try: {.code copilot --version} and {.code copilot login --status}"
      ))
    }
    return(.hal_empty_models_df())
  }

  ids <- vapply(models, function(m) m$modelId %||% NA_character_, character(1))
  names_ <- vapply(models, function(m) m$name %||% NA_character_, character(1))
  desc <- vapply(models, function(m) m$description %||% NA_character_, character(1))
  usage <- vapply(models, function(m) {
    as.character(m$`_meta`$copilotUsage %||% NA)
  }, character(1))
  ctx <- vapply(seq_along(models), function(i) {
    m <- models[[i]]
    as.integer(m$`_meta`$contextWindow %||%
                 .hal_context_window_for(m$modelId %||% NA_character_))
  }, integer(1))

  data.frame(
    id = ids,
    name = names_,
    description = desc,
    provider = .hal_provider_from_id(ids),
    family = .hal_family_from_id(ids),
    usage = .hal_normalize_usage(usage),
    multiplier = .hal_usage_to_numeric(usage),
    is_default = !is.na(ids) & ids == current_id,
    context_window = ctx,
    release_date = .hal_release_date_for(ids),
    stringsAsFactors = FALSE
  )
}

#' Inspect a single model in detail
#'
#' Returns the full per-model record, including the raw `_meta` payload from
#' the ACP server. Useful for debugging or accessing fields that
#' [hal_models()] doesn't surface as columns.
#'
#' @param id Model identifier. If `NULL`, prints available ids and returns
#'   them invisibly.
#' @param client A [HalClient] instance, or `NULL` to create one.
#' @return A list with `id`, `name`, `description`, `provider`, `family`,
#'   `usage`, `multiplier`, `is_default`, `context_window`, `release_date`,
#'   and `meta` (raw `_meta` list from the server, or `NULL` for Claude).
#'
#' @examples
#' \dontrun{
#' ids <- hal_model_info()   # print available ids
#' hal_model_info(ids[[1]])  # inspect the first one
#' }
#' @export
hal_model_info <- function(id = NULL, client = NULL) {
  if (is.null(id)) {
    df <- hal_models(client = client)
    cli::cli_inform(c(
      "i" = "Pass an id from this list:",
      stats::setNames(paste0("{.val ", df$id, "}"), rep(" ", nrow(df)))
    ))
    return(invisible(df$id))
  }
  if (is.null(client) && identical(.hal_backend(), "claude")) {
    df <- .hal_claude_models()
    row <- df[df$id == id, , drop = FALSE]
    if (nrow(row) == 0) {
      cli::cli_abort("Unknown Claude model id: {.val {id}}")
    }
    return(c(as.list(row), list(meta = NULL)))
  }

  if (is.null(client) && identical(.hal_backend(), "vscode")) {
    df <- .hal_vscode_models()
    row <- df[df$id == id, , drop = FALSE]
    if (nrow(row) == 0) {
      cli::cli_abort("Unknown vscode.lm model id: {.val {id}}")
    }
    return(c(as.list(row), list(meta = NULL)))
  }

  own_client <- is.null(client)
  client <- client %||% HalClient$new(quiet = TRUE)
  on.exit(if (own_client) client$stop(), add = TRUE)

  session <- client$new_session()
  models <- session$models$availableModels %||% list()
  current_id <- session$models$currentModelId %||% NA_character_

  match_idx <- which(vapply(models, function(m) {
    identical(m$modelId, id)
  }, logical(1)))

  if (length(match_idx) == 0) {
    cli::cli_abort(c(
      "Unknown model id: {.val {id}}",
      "i" = "See {.code hal_models()} for available ids."
    ))
  }

  m <- models[[match_idx[1]]]
  usage <- as.character(m$`_meta`$copilotUsage %||% NA)
  list(
    id = m$modelId,
    name = m$name %||% NA_character_,
    description = m$description %||% NA_character_,
    provider = .hal_provider_from_id(m$modelId),
    family = .hal_family_from_id(m$modelId),
    usage = .hal_normalize_usage(usage),
    multiplier = .hal_usage_to_numeric(usage),
    is_default = identical(m$modelId, current_id),
    context_window = as.integer(m$`_meta`$contextWindow %||%
                                   .hal_context_window_for(m$modelId)),
    release_date = .hal_release_date_for(m$modelId),
    meta = m$`_meta`
  )
}

# ---- helpers ---------------------------------------------------------------

.hal_empty_models_df <- function() {
  data.frame(
    id = character(), name = character(), description = character(),
    provider = character(), family = character(), usage = character(),
    multiplier = numeric(), is_default = logical(),
    context_window = integer(), release_date = as.Date(character()),
    stringsAsFactors = FALSE
  )
}

.hal_provider_from_id <- function(ids) {
  vapply(ids, function(x) {
    if (is.na(x)) return(NA_character_)
    if (grepl("^gpt", x, ignore.case = TRUE)) return("openai")
    if (grepl("^claude", x, ignore.case = TRUE)) return("anthropic")
    if (grepl("^gemini", x, ignore.case = TRUE)) return("google")
    "unknown"
  }, character(1), USE.NAMES = FALSE)
}

.hal_family_from_id <- function(ids) {
  vapply(ids, function(x) {
    if (is.na(x)) return(NA_character_)
    xl <- tolower(x)
    if (grepl("opus", xl)) return("heavy")
    if (grepl("mini|haiku|nano|flash", xl)) return("light")
    "standard"
  }, character(1), USE.NAMES = FALSE)
}

.hal_normalize_usage <- function(usage) {
  ifelse(
    is.na(usage) | usage == "NA",
    NA_character_,
    ifelse(grepl("x$", usage), usage, paste0(usage, "x"))
  )
}

.hal_usage_to_numeric <- function(usage) {
  num <- suppressWarnings(as.numeric(sub("x$", "", usage)))
  num
}

# Static release dates for known models. Maintained manually; missing ids
# return NA. Update when new models are added to the Copilot fleet.
.hal_model_release_dates <- function() {
  c(
    "gpt-4.1"             = "2025-04-14",
    "gpt-5-mini"          = "2025-08-07",
    "gpt-5.1"             = "2025-09-01",
    "gpt-5.1-codex"       = "2025-09-01",
    "gpt-5.1-codex-mini"  = "2025-09-01",
    "gpt-5.1-codex-max"   = "2025-09-15",
    "gpt-5.2"             = "2025-10-01",
    "gpt-5.2-codex"       = "2025-10-01",
    "gpt-5.3-codex"       = "2025-11-01",
    "gpt-5.4"             = "2025-12-01",
    "gpt-5.4-mini"        = "2025-12-01",
    "gpt-5.5"             = "2026-02-01",
    "claude-opus-4.7"     = "2025-12-15",
    "claude-haiku-4.5"    = "2025-10-15",
    "claude-haiku-4-5" = "2025-10-01",
    "claude-sonnet-4"     = "2025-05-22",
    "claude-sonnet-4.5"   = "2025-08-15",
    "claude-sonnet-4.6"   = "2025-09-29",
    "claude-sonnet-4-6"   = "2025-09-29",
    "claude-opus-4.5"     = "2025-08-15",
    "claude-opus-4.6"     = "2025-09-29",
    "claude-opus-4-7"     = "2025-12-15",
    "gemini-3-pro-preview" = "2025-11-15"
  )
}

# Static context windows (tokens). Copilot's ACP payload doesn't include
# this, so we maintain a known-models table. Missing ids -> NA.
.hal_model_context_windows <- function() {
  c(
    "gpt-4.1"             = 128000L,
    "gpt-5-mini"          = 272000L,
    "gpt-5.1"             = 272000L,
    "gpt-5.1-codex"       = 272000L,
    "gpt-5.1-codex-mini"  = 272000L,
    "gpt-5.1-codex-max"   = 272000L,
    "gpt-5.2"             = 272000L,
    "gpt-5.2-codex"       = 272000L,
    "gpt-5.3-codex"       = 272000L,
    "gpt-5.4"             = 272000L,
    "gpt-5.4-mini"        = 272000L,
    "gpt-5.5"             = 272000L,
    "claude-haiku-4.5"    = 200000L,
    "claude-haiku-4-5" = 200000L,
    "claude-sonnet-4"     = 200000L,
    "claude-sonnet-4.5"   = 200000L,
    "claude-sonnet-4.6"   = 200000L,
    "claude-sonnet-4-6"   = 200000L,
    "claude-opus-4.5"     = 200000L,
    "claude-opus-4.6"     = 200000L,
    "claude-opus-4.7"     = 200000L,
    "claude-opus-4-7"     = 200000L,
    # Newer ids served by Copilot / vscode.lm. Both host these near 200K rather
    # than Anthropic's native 1M -- values below match what the bridge reports.
    "claude-opus-4.8"     = 200000L,
    "claude-opus-5"       = 200000L,
    "claude-sonnet-5"     = 200000L,
    "gemini-3-pro-preview" = 1000000L
  )
}

.hal_context_window_for <- function(ids) {
  if (length(ids) == 0) return(integer(0))
  table <- .hal_model_context_windows()
  out <- rep(NA_integer_, length(ids))
  hits <- !is.na(ids) & ids %in% names(table)
  out[hits] <- unname(table[ids[hits]])
  out
}

.hal_release_date_for <- function(ids) {
  table <- .hal_model_release_dates()
  out <- as.Date(rep(NA_character_, length(ids)))
  hits <- ids %in% names(table)
  out[hits] <- as.Date(table[ids[hits]])
  out
}

# Live model list for the vscode backend, fetched from hal-bridge.
#
# The bridge returns whatever `vscode.lm.selectChatModels({})` produces for
# the user's Copilot session: id, vendor, family, name, version,
# maxInputTokens. Billing tier (usage / multiplier) isn't exposed by
# vscode.lm, so it's reported as NA. is_default is set to TRUE for the
# entry whose id matches the backend's default model ("auto").
.hal_vscode_models <- function() {
  info <- tryCatch(.hal_bridge_discover(), error = function(e) NULL)
  if (is.null(info)) return(.hal_empty_models_df())

  # Bridge >= 0.1.1 requires bearer token on /models; older bridges ignore it.
  mdl <- tryCatch(
    .hal_bridge_get_json(info$port, "/models", token = info$token),
    error = function(e) NULL
  )
  if (is.null(mdl) || length(mdl) == 0) return(.hal_empty_models_df())

  ids   <- vapply(mdl, function(m) m$id %||% NA_character_, character(1))
  names_<- vapply(mdl, function(m) m$name %||% NA_character_, character(1))
  ctx   <- vapply(mdl, function(m) {
    as.integer(m$maxInputTokens %||% NA_integer_)
  }, integer(1))

  default_id <- .hal_default_model("vscode")
  data.frame(
    id = ids,
    name = names_,
    description = rep(NA_character_, length(ids)),
    provider = .hal_provider_from_id(ids),
    family = .hal_family_from_id(ids),
    usage = rep(NA_character_, length(ids)),
    multiplier = rep(NA_real_, length(ids)),
    is_default = !is.na(ids) & ids == default_id,
    context_window = ctx,
    release_date = .hal_release_date_for(ids),
    stringsAsFactors = FALSE
  )
}


# Alias-based model list for the Claude backend. The Claude CLI exposes no
# model-list command, but `--model` accepts these aliases, which Anthropic
# resolves to the latest model of each tier server-side -- so they never go
# stale. Pass a concrete id (e.g. "claude-opus-5") to `model=` to pin a
# version; the CLI validates it. `is_default` tracks the backend default.
.hal_claude_models <- function() {
  ids <- c("haiku", "sonnet", "opus")
  data.frame(
    id = ids,                          # pass straight to --model; always latest
    name = c("Claude Haiku (latest)",
             "Claude Sonnet (latest)",
             "Claude Opus (latest)"),
    description = c(
      "Fast, cheap. Alias for the latest Haiku (currently claude-haiku-4-5).",
      "Balanced. Alias for the latest Sonnet (currently claude-sonnet-5).",
      "Heaviest reasoning. Alias for the latest Opus (currently claude-opus-5)."
    ),
    provider = "anthropic",
    family = c("light", "standard", "heavy"),
    usage = c("1x", "3x", "15x"),      # tier of the resolved model (approximate)
    multiplier = c(1, 3, 15),
    is_default = ids == .hal_default_model("claude"),
    # Native Anthropic context windows: Sonnet/Opus are 1M, Haiku is 200K.
    # These are larger than the same families get through Copilot/vscode.lm,
    # which cap their hosted variants near 200K (see .hal_model_context_windows).
    context_window = c(200000L, 1000000L, 1000000L),
    release_date = as.Date(rep(NA_character_, length(ids))),
    stringsAsFactors = FALSE
  )
}
