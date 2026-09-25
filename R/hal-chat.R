#' Create a Copilot chat session
#'
#' @description
#' Creates a new [HalChat] instance for conversing with GitHub Copilot
#' models. This is the recommended entry point -- equivalent to
#' `HalChat$new()` but follows tidyverse naming conventions.
#'
#' @param model Model identifier (e.g., `"claude-sonnet-5"`, `"gpt-5.2"`).
#'   Use [hal_models()] to see available models. If `NULL`, falls back to
#'   the `COPILOT_MODEL` environment variable, then the server default.
#' @param system_prompt System prompt string. Defaults to hal's built-in R
#'   assistant prompt. Pass a string to override, or set via
#'   `hal_configure(system_prompt = "...")`.
#' @param client An existing [HalClient] instance, or `NULL` to create
#'   one automatically.
#' @param echo Echo mode: `"none"`, `"output"`, or `"all"`. Defaults to
#'   `"output"` in interactive sessions, `"none"` otherwise.
#' @param on_text Callback for streaming text chunks: `function(chunk)`.
#'   Called for each `agent_message_chunk` as it arrives.
#' @param on_tool_call Callback for tool call events: `function(tool_call)`.
#'   Called with a `hal_tool_call` object on tool start and completion.
#' @param on_thought Callback for thought chunks: `function(chunk)`.
#'   Called for each `agent_thought_chunk` as it arrives.
#' @param mode Session mode: `"agent"` (default), `"plan"`, or `"autopilot"`.
#'   Applied after the session is created on the first prompt.
#' @param permission_policy Permission policy for tool calls. One of
#'   `"auto-allow"` (default), `"auto-deny"`, `"ask"` (interactive prompt;
#'   denies in non-interactive sessions; vscode backend only), or a function
#'   receiving `list(backend, tool_name, input)` and returning a string
#'   containing `"allow"` or `"deny"`. The copilot backend additionally
#'   accepts functions returning an ACP option id. Ignored if `client` is
#'   provided.
#' @param quiet Logical; suppress informational messages (default `FALSE`).
#'
#' @return A [HalChat] object.
#'
#' @seealso [hal_models()], [hal_client()], [hal_available()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Default model
#' chat <- hal_chat()
#' chat$chat("Explain the pipe operator in R")
#'
#' # Specific model
#' chat <- hal_chat(model = "gpt-5.2")
#' chat$chat("Hello!")
#'
#' # With system prompt
#' chat <- hal_chat(
#'   model = "claude-haiku-4.5",
#'   system_prompt = "Reply concisely in one sentence."
#' )
#' chat$chat("What is R?")
#'
#' # Plan mode
#' chat <- hal_chat(mode = "plan")
#' chat$chat("Create an analysis plan for this dataset")
#'
#' # With streaming callbacks
#' chat <- hal_chat(
#'   on_text = function(chunk) cat(chunk),
#'   on_tool_call = function(tc) message("Tool: ", tc$title)
#' )
#'
#' # Read-only mode (deny file writes and shell commands)
#' chat <- hal_chat(permission_policy = "auto-deny")
#' chat$chat("What files are in R/?")
#' }
hal_chat <- function(model = NULL, system_prompt = NULL, client = NULL,
                         echo = NULL, on_text = NULL, on_tool_call = NULL,
                         on_thought = NULL, mode = NULL,
                         permission_policy = "auto-allow",
                         quiet = FALSE) {
  system_prompt <- system_prompt %||% .hal_default_system_prompt(mode = mode)

  HalChat$new(
    model = model,
    system_prompt = system_prompt,
    client = client,
    echo = echo,
    on_text = on_text,
    on_tool_call = on_tool_call,
    on_thought = on_thought,
    mode = mode,
    permission_policy = permission_policy,
    quiet = quiet
  )
}
