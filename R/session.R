# ==============================================================================
# hal Session State -- Global singleton for convenience functions
# ==============================================================================
#
# Enables functional API: hal_chat("hello") without creating objects.
# The session holds a HalChat instance + metadata (eval env, tool tracking,
# spawn state).

# Module-level session environment (persists for R session lifetime)
.hal_session_env <- new.env(parent = emptyenv())

#' Get the active hal session
#'
#' Returns the session environment, creating it if needed.
#'
#' @return The session environment.
#' @keywords internal
#' @noRd
.hal_get_session <- function() {
  .hal_session_env
}

#' Initialize a hal session
#'
#' Creates or resets the global HalChat instance. Called lazily by
#' convenience functions or explicitly via `hal_reset()`.
#'
#' @param model Model identifier, or NULL for server default.
#' @param system_prompt System prompt, or NULL.
#' @param mode Session mode: `"agent"`, `"plan"`, or `"autopilot"`, or NULL
#'   for default (agent).
#' @param permissions Permission policy for the SDK agent.
#' @param force If TRUE, recreate even if a session exists.
#' @return The session environment (invisibly).
#' @keywords internal
#' @noRd
.hal_initialize <- function(model = NULL, system_prompt = NULL,
                                mode = NULL, permissions = NULL,
                                force = FALSE) {
  session <- .hal_get_session()

  if (!is.null(session$chat) && !force) return(invisible(session))

  # Clean up previous session
  if (!is.null(session$chat)) {
    tryCatch(session$chat$get_client()$stop(), error = function(e) NULL)
  }

  # Resolve model from option/env; backend factory fills in a sensible default
  # (Copilot: server default; Claude: Haiku 4.5).
  model <- model %||%
    getOption("hal.default_model") %||%
    Sys.getenv("COPILOT_MODEL", unset = NA_character_)
  if (is.na(model)) model <- NULL

  # Resolve mode from option (before system prompt, so prompt is mode-aware)
  mode <- mode %||% getOption("hal.mode", NULL)

  # Resolve system prompt + expand {user} placeholder
  system_prompt <- system_prompt %||%
    getOption("hal.system_prompt") %||%
    .hal_default_system_prompt(mode = mode)
  if (!is.null(system_prompt)) {
    system_prompt <- gsub("\\{user\\}", .hal_get_current_user(), system_prompt)
  }

  # Resolve permission policy and quiet from options
  perm <- permissions %||%
    getOption("hal.permission_policy", "auto-allow")
  sess_quiet <- isTRUE(getOption("hal.session_quiet", FALSE))

  # Persist function policies on the session so the Claude permission_prompt
  # bridge (and any other IPC handler) can find the user's callback. String
  # policies don't need this — they map to CLI flags directly.
  session$permission_policy_fn <- if (is.function(perm)) perm else NULL

  # Helper: close active thought block (called on thought->tool or thought->text)
  close_thought <- function() {
    if (isTRUE(session$thought_active)) {
      .hal_thought_close()
      session$thought_active <- FALSE
    }
  }

  # Build callbacks (controlled by hal.show_thoughts option)
  thought_cb <- NULL
  tool_cb <- NULL
  text_cb <- NULL

  if (isTRUE(getOption("hal.show_thoughts", FALSE))) {
    thought_cb <- function(chunk) {
      if (!isTRUE(session$thought_active)) {
        .hal_thought_open()
        session$thought_active <- TRUE
      }
      .hal_thought_chunk(chunk)
    }

    # Close thought block when tool calls or text output begin
    tool_cb <- function(tc) close_thought()
    text_cb <- function(chunk) close_thought()
  }

  # Build HalChat -- echo = "none" because hal() handles display
  session$chat <- HalChat$new(
    model = model,
    system_prompt = system_prompt,
    echo = "none",
    on_thought = thought_cb,
    on_tool_call = tool_cb,
    on_text = text_cb,
    mode = mode,
    permission_policy = perm,
    quiet = sess_quiet
  )

  # Session metadata
  session$model <- model
  session$eval_caller_env <- globalenv()
  session$eval_r_assigned <- character()
  session$eval_tools_registered <- FALSE
  session$tool_names <- character()
  session$spawn_count <- 0L
  session$spawn_log <- list()
  session$data_notice_shown <- FALSE
  session$ipc_dir <- NULL
  session$memory_injected <- FALSE
  session$usage_log <- list()
  session$model_tiers <- NULL

  # Always register eval_r (inert without system prompt nudge from use_env).
  # Works on both Copilot (via ACP + --additional-mcp-config) and Claude
  # (via --mcp-config + --strict-mcp-config).
  .hal_ensure_eval_tools(session$chat)

  # Data transmission notice
  .hal_data_notice()

  invisible(session)
}

#' Ensure a session exists (lazy init)
#'
#' @return The session environment.
#' @keywords internal
#' @noRd
.hal_ensure_session <- function() {
  session <- .hal_get_session()
  if (is.null(session$chat)) {
    .hal_initialize()
  }
  session
}

#' Reset the hal session
#'
#' Destroys the current session and starts fresh.
#'
#' @param mode Session mode: `"agent"` (default), `"plan"`, or `"autopilot"`.
#'   If NULL, uses the default (agent).
#'
#' @return Invisibly returns `NULL`, called for its side effect.
#'
#' @examples
#' \dontrun{
#' hal_reset()               # fresh session on the same backend
#' hal_reset(mode = "plan")  # restart in plan mode (copilot backend)
#' }
#' @export
hal_reset <- function(mode = NULL) {
  .hal_initialize(mode = mode, force = TRUE)
  cli::cli_alert_success("hal session reset.")
  invisible(NULL)
}

#' Display a one-time data transmission notice
#'
#' @keywords internal
#' @noRd
.hal_data_notice <- function() {
  session <- .hal_get_session()
  if (isTRUE(session$data_notice_shown)) return(invisible(NULL))
  if (isTRUE(getOption("hal.init_quiet", FALSE))) return(invisible(NULL))
  session$data_notice_shown <- TRUE
  destination <- switch(.hal_backend(),
    copilot = "GitHub Copilot API",
    claude  = "Anthropic Claude API (via Claude Code)",
    vscode  = "model behind your Positron Copilot sign-in (via vscode.lm)",
    "configured model provider"
  )
  cli::cli_inform(c(
    "i" = "hal sends prompts and tool results to the {destination}.",
    "i" = "See {.code hal_config()} for governance controls."
  ))
}
