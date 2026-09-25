#' GitHub Copilot Chat Session
#'
#' @description
#' High-level chat interface to GitHub Copilot models. Manages conversation
#' history, tool registration, and streaming. API surface mirrors
#' ellmer::Chat for familiarity.
#'
#' Uses [HalClient] for JSON-RPC transport to the Copilot SDK CLI,
#' which handles model-specific format translation internally -- no proxy
#' translation bugs.
#'
#' The ACP server maintains conversation history server-side. Turns are
#' also tracked locally so you can inspect them with `$get_turns()`.
#'
#' @seealso [HalClient] for the low-level transport layer,
#'   [hal_models()] for available models.
#'
#' @examples
#' \dontrun{
#' # Preferred: use hal_chat()
#' chat <- hal_chat()
#' chat$chat("Explain the pipe operator in R")
#'
#' # Or use R6 constructor directly
#' chat <- HalChat$new(model = "gpt-5.2")
#' chat$chat("Hello!")
#' }
#'
#' @return An [R6][R6::R6Class] object of class `HalChat`. Methods return
#'   values as documented per method; construct with `HalChat$new()`.
#' @export
HalChat <- R6::R6Class(
  "HalChat",
  public = list(

    #' @description Create a new Copilot chat session.
    #' @param model Model identifier (e.g., `"claude-sonnet-5"`, `"gpt-5.2"`).
    #'   If `NULL`, uses the server default.
    #' @param system_prompt System prompt string.
    #' @param client A [HalClient] instance, or `NULL` to create one.
    #' @param echo Echo mode: `"none"`, `"output"`, or `"all"`.
    #' @param on_text Callback for streaming text chunks: `function(chunk)`.
    #' @param on_tool_call Callback for tool call events: `function(tool_call)`.
    #' @param on_thought Callback for thought chunks: `function(chunk)`.
    #' @param mode Session mode: `"agent"` (default), `"plan"`, or `"autopilot"`.
    #'   Applied after the session is created on the first prompt.
    #' @param permission_policy Permission policy for the agent: `"auto-allow"`,
    #'   `"auto-deny"`, or a custom function. Ignored if `client` is provided.
    #' @param quiet Logical; suppress informational messages (default: FALSE).
    initialize = function(
      model = NULL,
      system_prompt = NULL,
      client = NULL,
      echo = NULL,
      on_text = NULL,
      on_tool_call = NULL,
      on_thought = NULL,
      mode = NULL,
      permission_policy = "auto-allow",
      quiet = FALSE
    ) {
      private$model <- model
      private$system_prompt <- system_prompt
      private$mode <- mode
      private$client <- client %||% .hal_make_client(
        model = model,
        permission_policy = permission_policy,
        on_text = on_text,
        on_tool_call = on_tool_call,
        on_thought = on_thought,
        quiet = quiet
      )
      private$echo <- echo %||% if (interactive()) "output" else "none"
      private$turns <- list()
      private$tools <- list()
    },

    #' @description Send a message and get a response.
    #' @param ... Character strings, concatenated as the user message.
    #' @param timeout Timeout in seconds for the response.
    #' @return Assistant's text response (invisibly if `echo != "none"`).
    chat = function(..., timeout = NULL) {
      prompt <- paste0(c(...), collapse = " ")

      # Autopilot wants a generous timeout for multi-step reasoning. Otherwise
      # leave `timeout = NULL` so the client picks based on session state
      # (cold first call gets `hal.prompt_timeout_cold`, default 180; warm
      # turns get `hal.prompt_timeout`, default 60).
      if (is.null(timeout) && identical(private$mode, "autopilot")) {
        timeout <- 180
      }

      # Prepend system prompt on first turn (delimited so model treats it as
      # context, not user speech -- ACP has no system message slot)
      if (length(private$turns) == 0 && !is.null(private$system_prompt)) {
        prompt <- paste0(
          "<system-context>\n", private$system_prompt, "\n</system-context>\n\n",
          prompt
        )
      }

      user_turn <- turn("user", prompt)
      private$turns <- c(private$turns, list(user_turn))

      # Sync tools to client before first prompt (CLI needs them at startup)
      if (length(private$tools) > 0 && !private$tools_synced) {
        private$client$register_tools(private$tools)
        private$tools_synced <- TRUE
      }

      # Apply mode after tools are synced (set_mode triggers session creation,
      # which starts the CLI with MCP config already registered)
      if (!is.null(private$mode) && !private$mode_applied) {
        private$client$set_mode(private$mode)
        private$mode_applied <- TRUE
      }

      response <- private$client$prompt(prompt, timeout = timeout)

      # Echo tool calls if echo is "all"
      if (private$echo == "all" && length(response$tool_calls) > 0) {
        for (tc in response$tool_calls) {
          cat(format(tc), "\n")
        }
      }

      if (private$echo != "none") {
        cat(response$text, "\n")
      }

      asst_turn <- turn("assistant", response$text,
                         tool_calls = response$tool_calls,
                         thoughts = response$thoughts)
      private$turns <- c(private$turns, list(asst_turn))
      private$last_resp <- response

      if (private$echo == "none") response$text else invisible(response$text)
    },

    #' @description Register a tool for the model to call.
    #' @param tool A tool definition. Can be an `ellmer::ToolDef` or a list
    #'   with `name`, `description`, `parameters`, and `fun` fields.
    register_tool = function(tool) {
      tool_def <- as_tool_def(tool)
      private$tools[[tool_def$name]] <- tool_def
      private$tools_synced <- FALSE
      invisible(self)
    },

    #' @description Register multiple tools.
    #' @param tools A list of tool definitions.
    register_tools = function(tools) {
      for (tool in tools) {
        self$register_tool(tool)
      }
      invisible(self)
    },

    #' @description Get conversation turns.
    #' @param include_system_prompt Include the system prompt turn.
    #' @return List of turn objects.
    get_turns = function(include_system_prompt = FALSE) {
      if (include_system_prompt && !is.null(private$system_prompt)) {
        c(list(turn("system", private$system_prompt)), private$turns)
      } else {
        private$turns
      }
    },

    #' @description Get the full response from the last prompt.
    #' @return A `hal_response` object, or `NULL` if no responses yet.
    last_response = function() {
      private$last_resp
    },

    #' @description Get the last assistant turn.
    #' @return A `hal_turn` object, or `NULL` if no turns yet.
    last_turn = function() {
      asst_turns <- Filter(function(t) t$role == "assistant", private$turns)
      if (length(asst_turns) == 0) return(NULL)
      asst_turns[[length(asst_turns)]]
    },

    #' @description Get tool calls from the last assistant turn.
    #' @return List of `hal_tool_call` objects, or `NULL` if none.
    last_tool_calls = function() {
      resp <- private$last_resp
      if (is.null(resp)) return(NULL)
      resp$tool_calls
    },

    #' @description Switch models mid-session without losing context.
    #' @param model Model identifier (e.g., `"gpt-4.1"`, `"claude-haiku-4.5"`).
    #' @return Invisibly returns `self`.
    switch_model = function(model) {
      # Sync tools before switch_model triggers CLI start
      if (length(private$tools) > 0 && !private$tools_synced) {
        private$client$register_tools(private$tools)
        private$tools_synced <- TRUE
      }
      private$client$switch_model(model)
      invisible(self)
    },

    #' @description Set the session mode.
    #' @param mode `"agent"` (default), `"plan"`, or `"autopilot"`.
    #' @return Invisibly returns `self`.
    set_mode = function(mode = c("agent", "plan", "autopilot")) {
      private$client$set_mode(mode)
      invisible(self)
    },

    #' @description Cancel the current in-flight prompt.
    #'
    #' Stops the streaming loop and returns a partial response with
    #' `stop_reason = "interrupted"`. For interactive use, Ctrl+C (ESC in
    #' RStudio) during `$chat()` achieves the same effect automatically.
    #'
    #' Useful from Shiny observers, callbacks, or a second R session.
    #'
    #' @return Invisibly returns `self`.
    cancel = function() {
      private$client$cancel()
      invisible(self)
    },

    #' @description Get the model identifier.
    #' @return Character string.
    get_model = function() {
      private$model
    },

    #' @description Get the underlying client.
    #' @return A [HalClient] instance.
    get_client = function() {
      private$client
    }
  ),

  private = list(
    model = NULL,
    system_prompt = NULL,
    client = NULL,
    echo = NULL,
    mode = NULL,
    mode_applied = FALSE,
    turns = list(),
    tools = list(),
    tools_synced = FALSE,
    last_resp = NULL
  )
)
