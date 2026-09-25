# Mock Copilot CLI for offline testing
#
# Speaks NDJSON over stdio, same protocol as "copilot --acp".
# Scenario name passed as first command-line arg (default: "basic").
# No hal dependency — only jsonlite.

library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
# Filter out --acp (injected by HalClient$start())
args <- args[args != "--acp"]
scenario <- if (length(args) >= 1) args[1] else "basic"

# -- NDJSON I/O ---------------------------------------------------------------

send <- function(obj) {
  msg <- toJSON(obj, auto_unbox = TRUE, null = "null")
  cat(msg, "\n", sep = "", file = stdout())
  flush(stdout())
}

send_result <- function(id, result) {
  send(list(jsonrpc = "2.0", id = id, result = result))
}

send_error <- function(id, code, message) {
  send(list(jsonrpc = "2.0", id = id,
            error = list(code = code, message = message)))
}

send_notification <- function(method, params) {
  send(list(jsonrpc = "2.0", method = method, params = params))
}

# -- Session state -------------------------------------------------------------

session_id <- "mock-session-001"
prompt_count <- 0L
permission_id_counter <- 1000L

# -- Prompt response generators ------------------------------------------------

respond_basic <- function(id) {
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_message_chunk",
      content = list(type = "text", text = "Hello from ")
    )
  ))
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_message_chunk",
      content = list(type = "text", text = "MockCopilot.")
    )
  ))
  send_result(id, list(stopReason = "end_turn"))
}

respond_echo <- function(id, prompt_text) {
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_message_chunk",
      content = list(type = "text", text = prompt_text)
    )
  ))
  send_result(id, list(stopReason = "end_turn"))
}

respond_tool_call <- function(id) {
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_message_chunk",
      content = list(type = "text", text = "Let me check. ")
    )
  ))
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "tool_call",
      toolCallId = "tc-001",
      title = "view README.md",
      kind = "read",
      status = "pending",
      rawInput = list(path = "README.md")
    )
  ))
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "tool_call_update",
      toolCallId = "tc-001",
      status = "completed",
      rawOutput = list(content = "# hal\nA coding agent for R.")
    )
  ))
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_message_chunk",
      content = list(type = "text", text = "Done.")
    )
  ))
  send_result(id, list(stopReason = "end_turn"))
}

respond_thoughts <- function(id) {
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_thought_chunk",
      content = list(type = "text", text = "Let me think...")
    )
  ))
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_thought_chunk",
      content = list(type = "text", text = " Okay, I know.")
    )
  ))
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_message_chunk",
      content = list(type = "text", text = "Here is my answer.")
    )
  ))
  send_result(id, list(stopReason = "end_turn"))
}

respond_permission <- function(id, con) {
  # Send permission request (this has its own id)
  permission_id_counter <<- permission_id_counter + 1L
  perm_id <- permission_id_counter

  send(list(
    jsonrpc = "2.0",
    id = perm_id,
    method = "session/request_permission",
    params = list(
      sessionId = session_id,
      options = list(
        list(optionId = "allow-once", kind = "allow",
             description = "Allow this action"),
        list(optionId = "reject-once", kind = "reject",
             description = "Reject this action")
      ),
      toolCall = list(
        title = "edit test.R",
        kind = "write"
      )
    )
  ))

  # Wait for client's permission response
  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) return()
    line <- trimws(line)
    if (!nzchar(line)) next
    resp <- tryCatch(fromJSON(line, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (!is.null(resp) && !is.null(resp$id) && resp$id == perm_id) break
  }

  # Now send the actual prompt response
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_message_chunk",
      content = list(type = "text", text = "Permission handled.")
    )
  ))
  send_result(id, list(stopReason = "end_turn"))
}

respond_error <- function(id) {
  send_error(id, -32000, "Mock error: something went wrong")
}

respond_multi_turn <- function(id) {
  prompt_count <<- prompt_count + 1L
  text <- paste0("Turn ", prompt_count, " response.")
  send_notification("session/update", list(
    sessionId = session_id,
    update = list(
      sessionUpdate = "agent_message_chunk",
      content = list(type = "text", text = text)
    )
  ))
  send_result(id, list(stopReason = "end_turn"))
}

# -- Main handler --------------------------------------------------------------

handle <- function(msg, con) {
  method <- msg$method
  id <- msg$id

  # initialize
  if (identical(method, "initialize")) {
    send_result(id, list(
      protocolVersion = 1,
      agentCapabilities = structure(list(), names = character()),
      agentInfo = list(name = "MockCopilot", version = "0.0.1")
    ))
    return()
  }

  # initialized notification (no response)
  if (identical(method, "initialized")) return()

  # session/new
  if (identical(method, "session/new")) {
    send_result(id, list(
      sessionId = session_id,
      models = list(
        currentModelId = "mock-model",
        availableModels = list(
          list(modelId = "mock-model", name = "Mock Model",
               description = "For testing",
               `_meta` = list(copilotUsage = 0))
        )
      ),
      modes = structure(list(), names = character())
    ))
    return()
  }

  # session/prompt
  if (identical(method, "session/prompt")) {
    prompt_text <- tryCatch(
      msg$params$prompt[[1]]$text,
      error = function(e) ""
    )

    if (scenario == "basic")      respond_basic(id)
    else if (scenario == "echo")  respond_echo(id, prompt_text)
    else if (scenario == "tool_call") respond_tool_call(id)
    else if (scenario == "thoughts")  respond_thoughts(id)
    else if (scenario == "permission") respond_permission(id, con)
    else if (scenario == "error")  respond_error(id)
    else if (scenario == "multi_turn") respond_multi_turn(id)
    else respond_basic(id)
    return()
  }

  # session/set_model
  if (identical(method, "session/set_model")) {
    send_result(id, structure(list(), names = character()))
    return()
  }

  # session/set_mode
  if (identical(method, "session/set_mode")) {
    send_result(id, structure(list(), names = character()))
    return()
  }

  # Unknown method
  if (!is.null(id)) {
    send_error(id, -32601, paste0("Method not found: ", method))
  }
}

# -- Main loop -----------------------------------------------------------------

con <- file("stdin", open = "r")
while (TRUE) {
  line <- readLines(con, n = 1, warn = FALSE)
  if (length(line) == 0) break
  line <- trimws(line)
  if (!nzchar(line)) next
  msg <- tryCatch(fromJSON(line, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (!is.null(msg)) handle(msg, con)
}
