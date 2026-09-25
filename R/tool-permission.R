# ==============================================================================
# hal permission_prompt -- Bridge for user-approval flows on Claude backend
# ==============================================================================
#
# Claude Code's `--permission-prompt-tool <name>` flag tells Claude to call
# an MCP tool whenever a built-in tool needs permission. The tool receives
# `{ tool_name, input }` and must return `{ behavior: "allow"|"deny", ... }`.
#
# hal exposes that hook as `permission_prompt`, an MCP tool that proxies the
# request back to the parent R session via IPC (same pattern as eval_r). The
# parent invokes the user's `permission_policy` function with a synthesized
# Copilot-shaped `params` object so user code is portable across backends.
#
# Only used when `permission_policy` is a function. String policies map
# directly to `--permission-mode` and never round-trip through this tool.

# ------------------------------------------------------------------------------
# MCP tool definition
# ------------------------------------------------------------------------------

#' Build the permission_prompt MCP tool definition.
#'
#' The local `fun` is a safe deny-fallback in case Claude calls the tool when
#' IPC isn't wired up (shouldn't happen, but better than crashing). Real logic
#' runs in the parent via .hal_ipc_permission_fn().
#' @keywords internal
#' @noRd
.hal_permission_prompt_tool_def <- function() {
  list(
    name = "permission_prompt",
    description = paste(
      "hal-internal: bridges Claude Code permission requests back to the",
      "user's R session. Called automatically when --permission-prompt-tool",
      "is set. Do not invoke directly."
    ),
    fun = function(tool_name = "", input = list()) {
      # Local fallback: deny if reached without IPC.
      jsonlite::toJSON(
        list(behavior = "deny",
             message = "permission_prompt invoked without IPC; denying."),
        auto_unbox = TRUE
      )
    },
    parameters = list(
      type = "object",
      properties = list(
        tool_name = list(
          type = "string",
          description = "Name of the tool requesting permission."
        ),
        input = list(
          type = "object",
          description = "Arguments that would be passed to the tool.",
          properties = structure(list(), names = character()),
          additionalProperties = TRUE
        )
      ),
      required = list("tool_name")
    )
  )
}

# ------------------------------------------------------------------------------
# Parent-side IPC handler
# ------------------------------------------------------------------------------

#' Handle a permission_prompt request from the MCP subprocess.
#'
#' Passes the backend-honest shape to the user's policy function and maps the
#' returned decision to Claude's `{ behavior: "allow"|"deny" }` contract.
#'
#' Claude only forwards `tool_name` + `input` to the prompt tool — there is
#' no `kind` or `options` array. Don't fake those: hand the policy a clean
#' `list(backend = "claude", tool_name, input)`. Policies that need to be
#' portable across backends should branch on `params$backend`.
#'
#' @param request List with `kind = "permission"`, `tool_name`, `input`.
#' @return `list(result = <json string>, error = NULL)` for IPC framing.
#' @keywords internal
#' @noRd
.hal_ipc_permission_fn <- function(request) {
  session <- .hal_get_session()
  policy <- session$permission_policy_fn

  # Claude's --permission-prompt-tool contract:
  #   allow → {"behavior":"allow", "updatedInput": <input or modified>}
  #   deny  → {"behavior":"deny",  "message": "<reason>"}
  # Missing `updatedInput` on allow is treated as a malformed deny, which
  # surfaces as "missing message field when behavior is deny" from Claude's
  # own validator. Always echo the input back unless the policy customizes it.
  reply <- function(behavior, message = NULL, updated_input = NULL) {
    payload <- if (identical(behavior, "allow")) {
      # toJSON drops list() as []; force {} via named_list() when empty.
      ui <- updated_input %||% list()
      if (is.list(ui) && length(ui) == 0L) ui <- named_list()
      list(behavior = "allow", updatedInput = ui)
    } else if (is.null(message)) {
      list(behavior = "deny")
    } else {
      list(behavior = "deny", message = message)
    }
    list(
      result = as.character(jsonlite::toJSON(payload, auto_unbox = TRUE)),
      error = NULL
    )
  }

  raw_input <- request$input %||% list()
  tool_name <- request$tool_name %||% "(unknown)"

  if (!is.function(policy)) {
    return(reply("deny", "No permission policy function registered."))
  }

  params <- list(
    backend   = "claude",
    tool_name = tool_name,
    input     = raw_input
  )

  decision <- tryCatch(policy(params), error = function(e) {
    cli::cli_warn(c(
      "!" = "permission_policy function errored: {conditionMessage(e)}",
      "i" = "Denying request as a safety default."
    ))
    "reject-once"
  })

  if (!is.character(decision) || length(decision) != 1L) decision <- "reject-once"
  if (grepl("allow", decision, ignore.case = TRUE)) {
    reply("allow", updated_input = raw_input)
  } else {
    reply("deny", "Denied by hal permission policy.")
  }
}
