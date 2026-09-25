#' Create a Copilot ACP client
#'
#' @description
#' Creates a new [HalClient] instance for low-level ACP communication.
#' Most users should use [hal_chat()] instead.
#'
#' @param model Model identifier (e.g., `"claude-sonnet-5"`, `"gpt-5.2"`).
#'   Passed as `--model` to the CLI at startup. If `NULL`, falls back to the
#'   `COPILOT_MODEL` environment variable, then the server default.
#' @param cli_path Path to the Copilot CLI binary. If `NULL`, searches
#'   `PATH` and common install locations.
#' @param permission_policy How to handle tool permission requests:
#'   `"auto-allow"` (default) auto-approves, `"auto-deny"` auto-denies,
#'   `"ask"` prompts interactively (vscode backend only), or a function
#'   receiving permission params. The function returns an ACP option id
#'   on copilot, or a string containing `"allow"`/`"deny"` on
#'   vscode/claude.
#' @param on_text Callback for streaming text chunks: `function(chunk)`.
#' @param on_tool_call Callback for tool call events: `function(tool_call)`.
#' @param on_thought Callback for thought chunks: `function(chunk)`.
#' @param quiet Logical; suppress informational messages (default `FALSE`).
#'
#' @return A [HalClient] object.
#'
#' @seealso [hal_chat()], [hal_available()]
#'
#' @export
#' @examples
#' \dontrun{
#' client <- hal_client()
#' client$start()
#' client$handshake()
#' session <- client$new_session()
#' client$stop()
#' }
hal_client <- function(model = NULL, cli_path = NULL,
                           permission_policy = "auto-allow",
                           on_text = NULL, on_tool_call = NULL,
                           on_thought = NULL, quiet = FALSE) {
  HalClient$new(
    model = model,
    cli_path = cli_path,
    permission_policy = permission_policy,
    on_text = on_text,
    on_tool_call = on_tool_call,
    on_thought = on_thought,
    quiet = quiet
  )
}
