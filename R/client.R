#' GitHub Copilot SDK Client
#'
#' @description
#' Manages the Copilot CLI subprocess in ACP (Agent Client Protocol) mode
#' and handles JSON-RPC 2.0 communication over NDJSON stdio. This is the
#' low-level transport layer -- most users should use [HalChat] instead.
#'
#' The client spawns the Copilot CLI with `--acp`, which starts it as a
#' JSON-RPC server. Authentication uses ambient Copilot credentials from
#' any editor (VS Code, Positron, JetBrains).
#'
#' @seealso [HalChat] for the high-level chat interface,
#'   [hal_available()] to check CLI availability.
#'
#' @examples
#' \dontrun{
#' # Preferred: use hal_client()
#' client <- hal_client()
#' client$handshake()
#' session <- client$new_session()
#' client$stop()
#' }
#'
#' @return An [R6][R6::R6Class] object of class `HalClient`. Methods return
#'   values as documented per method; construct with `HalClient$new()`.
#' @export
HalClient <- R6::R6Class(
  "HalClient",
  public = list(

    #' @description Create a new Copilot SDK client.
    #' @param model Model identifier (e.g., `"claude-sonnet-5"`, `"gpt-5.2"`).
    #'   Passed as `--model` to the CLI at startup. If `NULL`, falls back to the
    #'   `COPILOT_MODEL` environment variable, then the server default.
    #' @param cli_path Path to the Copilot CLI binary. If `NULL`,
    #'   searches `PATH` and common install locations.
    #' @param permission_policy How to handle tool permission requests:
    #'   `"auto-allow"` (default) auto-approves all, `"auto-deny"` auto-denies,
    #'   or a function receiving the permission params and returning an option ID.
    #' @param on_text Callback function receiving each text chunk as it streams.
    #'   Signature: `function(chunk)`. Called for each `agent_message_chunk`.
    #' @param on_tool_call Callback function receiving tool call events.
    #'   Signature: `function(tool_call)`. Called on `tool_call` and
    #'   `tool_call_update` events with a `hal_tool_call` object.
    #' @param on_thought Callback function receiving thought chunks.
    #'   Signature: `function(chunk)`. Called for each `agent_thought_chunk`.
    #' @param quiet Logical; suppress informational messages during init,
    #'   handshake, and session creation (default: FALSE).
    initialize = function(model = NULL, cli_path = NULL,
                          permission_policy = "auto-allow",
                          on_text = NULL, on_tool_call = NULL,
                          on_thought = NULL, quiet = FALSE) {
      private$cli_info <- find_hal_cli(cli_path)
      private$model_id <- model %||% Sys.getenv("COPILOT_MODEL", unset = NA)
      if (is.na(private$model_id)) private$model_id <- NULL
      private$request_id <- 0L
      private$started <- FALSE
      private$initialized <- FALSE
      private$quiet <- isTRUE(quiet)
      private$permission_policy <- permission_policy
      private$registered_tools <- list()
      private$cb_on_text <- on_text
      private$cb_on_tool_call <- on_tool_call
      private$cb_on_thought <- on_thought
    },

    #' @description Start the Copilot CLI subprocess in ACP mode.
    #' @return Invisibly returns `self`.
    start = function() {
      if (private$started) {
        cli::cli_inform("Copilot client already running.")
        return(invisible(self))
      }

      info <- private$cli_info

      cli_args <- c(info$prefix_args, "--acp")
      if (!is.null(private$model_id)) {
        cli_args <- c(cli_args, "--model", private$model_id)
      }

      # Build MCP config if tools are registered
      if (length(private$registered_tools) > 0) {
        mcp <- build_mcp_config(private$registered_tools,
                                ipc_dir = private$ipc_dir,
                                backend = "copilot")
        private$mcp_config_path <- mcp$config_path
        private$mcp_script_path <- mcp$script_path
        private$mcp_tools_path <- mcp$tools_path
        cli_args <- c(cli_args,
                      "--additional-mcp-config",
                      paste0("@", mcp$config_path))
        cli::cli_inform(c(
          "i" = "MCP config: {length(private$registered_tools)} tool{?s} via --additional-mcp-config"
        ))
      }

      private$process <- processx::process$new(
        command = info$command,
        args = cli_args,
        stdin = "|",
        stdout = "|",
        stderr = "|",
        cleanup = TRUE,
        cleanup_tree = TRUE
      )

      # Poll until the process is responsive or clearly dead
      start_deadline <- Sys.time() + 5
      while (Sys.time() < start_deadline) {
        if (!private$process$is_alive()) {
          stderr_out <- private$process$read_all_error()
          cli::cli_abort(c(
            "Copilot CLI failed to start.",
            "x" = if (nzchar(stderr_out)) stderr_out else "Process exited immediately."
          ))
        }
        # Process is alive -- it's ready to accept input
        break
      }

      private$started <- TRUE
      if (!private$quiet) {
        cli::cli_inform(c(
          "v" = "Copilot ACP client started.",
          "i" = "Command: {info$command} {paste(cli_args, collapse = ' ')}"
        ))
      }

      invisible(self)
    },

    #' @description Stop the Copilot CLI subprocess.
    stop = function() {
      if (private$started && !is.null(private$process) && private$process$is_alive()) {
        private$process$kill()
      }
      private$started <- FALSE
      private$initialized <- FALSE
      private$session_id <- NULL
      # Clean up temp MCP files
      for (f in c(private$mcp_tools_path, private$mcp_script_path,
                   private$mcp_config_path)) {
        if (!is.null(f) && file.exists(f)) unlink(f)
      }
      private$mcp_tools_path <- NULL
      private$mcp_script_path <- NULL
      private$mcp_config_path <- NULL
      # Clean up IPC directory
      if (!is.null(private$ipc_dir) && dir.exists(private$ipc_dir)) {
        unlink(private$ipc_dir, recursive = TRUE)
      }
      private$ipc_dir <- NULL
      private$ipc_eval_fn <- NULL
      invisible(self)
    },

    #' @description Register tools to expose via MCP server.
    #'
    #' Tools must be registered before the first `$prompt()` call (before the
    #' CLI subprocess starts). The tools are passed via `--additional-mcp-config`
    #' at startup. Registering tools after the CLI is running triggers a warning.
    #'
    #' Tools are merged into the existing set by name. Call multiple times to
    #' accumulate tools from different sources.
    #'
    #' @param tools Named list of tool definitions.
    #' @return Invisibly returns `self`.
    register_tools = function(tools) {
      if (private$started) {
        cli::cli_warn(c(
          "!" = "Tools registered after CLI started will not take effect.",
          "i" = "Register tools before the first {.fun $chat} or {.fun $prompt} call."
        ))
      }
      # Merge by name so multiple register_tools() calls accumulate
      for (nm in names(tools)) {
        private$registered_tools[[nm]] <- tools[[nm]]
      }
      invisible(self)
    },

    #' @description Perform the ACP `initialize` handshake.
    #'
    #' Sends the `initialize` request and `initialized` notification.
    #' Called automatically on first use if needed.
    #'
    #' @param timeout Timeout in seconds.
    #' @return The initialize result (agent info and capabilities).
    handshake = function(timeout = 15) {
      if (private$initialized) return(invisible(private$agent_info))
      if (!private$started) self$start()

      result <- self$request(
        method = "initialize",
        params = list(
          protocolVersion = 1,
          clientCapabilities = named_list()
        ),
        timeout = timeout
      )

      # Send initialized notification (required by protocol)
      self$notify("initialized")

      private$initialized <- TRUE
      private$agent_info <- result

      if (!private$quiet) {
        cli::cli_inform(c(
          "v" = "ACP handshake complete.",
          "i" = "Agent: {result$agentInfo$name} v{result$agentInfo$version}"
        ))
      }

      invisible(result)
    },

    #' @description Create a new ACP session.
    #'
    #' Sends `session/new` to create a session. The session holds conversation
    #' state server-side. Automatically performs the handshake if needed.
    #'
    #' Custom tools are configured via `--additional-mcp-config` at CLI startup
    #' (not via `mcpServers` in this call, which is broken per CLI issue #1040).
    #'
    #' @param cwd Working directory to report to the server.
    #' @param timeout Timeout in seconds.
    #' @return The session result (session ID, available models, modes).
    new_session = function(cwd = getwd(), timeout = 15) {
      if (!private$initialized) self$handshake()

      params <- list(
        cwd = normalizePath(cwd, winslash = "/"),
        mcpServers = list()
      )

      result <- self$request(
        method = "session/new",
        params = params,
        timeout = timeout
      )

      private$session_id <- result$sessionId

      if (!private$quiet) {
        current_model <- result$models$currentModelId
        n_models <- length(result$models$availableModels)
        if (!is.null(current_model)) {
          cli::cli_inform(c(
            "v" = "Session created: {.val {result$sessionId}}",
            "i" = "Model: {.val {current_model}} ({n_models} available)"
          ))
        } else {
          cli::cli_inform(c(
            "v" = "Session created: {.val {result$sessionId}}"
          ))
        }
      }

      invisible(result)
    },

    #' @description Send a prompt and collect the streamed response.
    #'
    #' Sends `session/prompt` and reads `session/update` notifications until
    #' the final response arrives. Automatically creates a session if needed.
    #'
    #' @param text The prompt text.
    #' @param timeout Timeout in seconds.
    #' @return A `hal_response` with `text`, `stop_reason`, `tool_calls`,
    #'   `thoughts`, and `events`.
    prompt = function(text, timeout = NULL) {
      if (is.null(timeout)) timeout <- getOption("hal.prompt_timeout", 60)
      if (is.null(private$session_id)) self$new_session()

      id <- private$next_id()
      private$current_prompt_id <- id
      private$cancelled <- FALSE

      msg <- jsonlite::toJSON(
        list(
          jsonrpc = "2.0",
          id = id,
          method = "session/prompt",
          params = list(
            sessionId = private$session_id,
            prompt = list(
              list(type = "text", text = text)
            )
          )
        ),
        auto_unbox = TRUE,
        null = "null"
      )

      private$send_ndjson(msg)
      on.exit(private$current_prompt_id <- NULL)
      tryCatch(
        private$receive_prompt_response(id = id, timeout = timeout),
        interrupt = function(cnd) {
          private$cancelled <- TRUE
          cli::cli_inform(c("!" = "Prompt interrupted."))
          # Return whatever has been collected so far
          private$partial_response
        }
      )
    },

    #' @description Send a JSON-RPC request and wait for a response.
    #' @param method JSON-RPC method name.
    #' @param params Named list of parameters.
    #' @param timeout Timeout in seconds.
    #' @return Parsed JSON response result.
    request = function(method, params = list(), timeout = 60) {
      if (!private$started) self$start()

      id <- private$next_id()

      msg <- jsonlite::toJSON(
        list(
          jsonrpc = "2.0",
          id = id,
          method = method,
          params = params
        ),
        auto_unbox = TRUE,
        null = "null"
      )

      private$send_ndjson(msg)
      private$receive_response(id = id, timeout = timeout)
    },

    #' @description Send a JSON-RPC notification (no response expected).
    #' @param method JSON-RPC method name.
    #' @param params Named list of parameters.
    notify = function(method, params = list()) {
      if (!private$started) self$start()

      msg <- jsonlite::toJSON(
        list(
          jsonrpc = "2.0",
          method = method,
          params = params
        ),
        auto_unbox = TRUE,
        null = "null"
      )

      private$send_ndjson(msg)
      invisible(self)
    },

    #' @description Switch models mid-session.
    #'
    #' Changes the active model without losing conversation context.
    #' Use [hal_models()] to see available model IDs.
    #'
    #' @param model Model identifier (e.g., `"gpt-4.1"`, `"claude-haiku-4.5"`).
    #' @param timeout Timeout in seconds.
    #' @return Invisibly returns `self`.
    switch_model = function(model, timeout = 10) {
      if (is.null(private$session_id)) self$new_session()

      self$request(
        method = "session/set_model",
        params = list(
          sessionId = private$session_id,
          modelId = model
        ),
        timeout = timeout
      )

      cli::cli_inform(c("v" = "Model switched to {.val {model}}."))
      invisible(self)
    },

    #' @description Set the session mode.
    #'
    #' Switches between Agent, Plan, and Autopilot modes.
    #' - **Agent**: Default conversational mode.
    #' - **Plan**: Multi-step planning mode with structured output.
    #' - **Autopilot**: Autonomous mode that runs until task completion
    #'   without user interaction (experimental).
    #'
    #' @param mode `"agent"`, `"plan"`, or `"autopilot"`.
    #' @param timeout Timeout in seconds.
    #' @return Invisibly returns `self`.
    set_mode = function(mode = c("agent", "plan", "autopilot"), timeout = 10) {
      mode <- match.arg(mode)
      if (is.null(private$session_id)) self$new_session()

      mode_uri <- paste0(
        "https://agentclientprotocol.com/protocol/session-modes#", mode
      )

      self$request(
        method = "session/set_mode",
        params = list(
          sessionId = private$session_id,
          modeId = mode_uri
        ),
        timeout = timeout
      )

      cli::cli_inform(c("v" = "Mode set to {.val {mode}}."))
      invisible(self)
    },

    #' @description Get the current session ID.
    #' @return Character string, or `NULL` if no session is active.
    get_session_id = function() {
      private$session_id
    },

    #' @description Create a temporary new session, saving the current one.
    #'
    #' Used internally by disposable verbs (`hal_ask`) to get history isolation
    #' on the same CLI process. Call `restore_session()` to switch back.
    #'
    #' @return The saved (previous) session ID (invisibly).
    swap_session = function() {
      saved <- private$session_id
      private$session_id <- NULL
      self$new_session()
      invisible(saved)
    },

    #' @description Restore a previously saved session ID.
    #' @param session_id The session ID returned by `swap_session()`.
    #' @return Invisibly returns `self`.
    restore_session = function(session_id) {
      private$session_id <- session_id
      invisible(self)
    },

    #' @description Cancel the current in-flight prompt.
    #'
    #' Signals the streaming loop to stop and return a partial response with
    #' `stop_reason = "interrupted"`. Safe to call from callbacks, Shiny
    #' observers, or a second thread. Does nothing if no prompt is active.
    #'
    #' For interactive use, pressing Ctrl+C (ESC in RStudio) during a prompt
    #' achieves the same effect automatically.
    #'
    #' @return Invisibly returns `self`.
    cancel = function() {
      private$cancelled <- TRUE
      invisible(self)
    },

    #' @description Configure IPC for live eval_r execution.
    #'
    #' When set, the polling loop checks for eval_r requests from the MCP
    #' subprocess and executes them in the user's R session via `eval_fn`.
    #'
    #' @param ipc_dir Path to the IPC directory for request/response files.
    #' @param eval_fn Function taking a code string and returning
    #'   `list(result = "...", error = NULL)` or `list(result = NULL, error = "...")`.
    #' @param permission_fn Optional handler for permission requests (used by
    #'   the Claude permission_prompt bridge). Takes the parsed request object
    #'   and returns the same `list(result, error)` shape as `eval_fn`.
    #' @return Invisibly returns `self`.
    set_ipc = function(ipc_dir, eval_fn = NULL, permission_fn = NULL) {
      private$ipc_dir <- ipc_dir
      private$ipc_eval_fn <- eval_fn
      private$ipc_permission_fn <- permission_fn
      invisible(self)
    },

    #' @description Check if the client subprocess is running.
    #' @return Logical.
    is_alive = function() {
      private$started && !is.null(private$process) && private$process$is_alive()
    },

    #' @description Read any available stderr output (for debugging).
    #' @return Character string.
    read_stderr = function() {
      if (!is.null(private$process)) {
        private$process$read_error()
      } else {
        ""
      }
    }
  ),

  private = list(
    cli_info = NULL,
    model_id = NULL,
    process = NULL,
    request_id = 0L,
    started = FALSE,
    initialized = FALSE,
    quiet = FALSE,
    session_id = NULL,
    agent_info = NULL,
    permission_policy = "auto-allow",
    registered_tools = list(),
    cancelled = FALSE,
    current_prompt_id = NULL,
    partial_response = NULL,
    cb_on_text = NULL,
    cb_on_tool_call = NULL,
    cb_on_thought = NULL,
    mcp_config_path = NULL,
    mcp_tools_path = NULL,
    mcp_script_path = NULL,
    ipc_dir = NULL,
    ipc_eval_fn = NULL,
    ipc_permission_fn = NULL,

    next_id = function() {
      private$request_id <- private$request_id + 1L
      private$request_id
    },

    # NDJSON framing: one JSON object per line, terminated by \n
    send_ndjson = function(msg) {
      payload <- paste0(as.character(msg), "\n")
      private$process$write_input(payload)
    },

    # Parse buffered NDJSON lines from a chunk, returns list of parsed objects
    # and remaining buffer
    parse_ndjson_chunk = function(buffer, chunk) {
      buffer <- paste0(buffer, chunk)
      lines <- strsplit(buffer, "\n", fixed = TRUE)[[1]]

      # Last element might be incomplete (no trailing \n)
      if (!endsWith(buffer, "\n")) {
        remaining <- lines[length(lines)]
        lines <- lines[-length(lines)]
      } else {
        remaining <- ""
      }

      parsed <- list()
      for (line in lines) {
        line <- trimws(line)
        if (!nzchar(line)) next

        obj <- tryCatch(
          jsonlite::fromJSON(line, simplifyVector = FALSE),
          error = function(e) NULL
        )
        if (!is.null(obj)) {
          parsed <- c(parsed, list(obj))
        }
      }

      list(parsed = parsed, buffer = remaining)
    },

    # Read NDJSON lines until we find a response matching `id`
    # Discards any notifications (used for initialize, session/new)
    receive_response = function(id, timeout = 60) {
      deadline <- Sys.time() + timeout
      buffer <- ""

      while (Sys.time() < deadline) {
        chunk <- private$read_stdout(500)
        if (nzchar(chunk)) {
          result <- private$parse_ndjson_chunk(buffer, chunk)
          buffer <- result$buffer

          for (obj in result$parsed) {
            if (!is.null(obj$id) && obj$id == id) {
              if (!is.null(obj$error)) {
                cli::cli_abort(c(
                  "Copilot ACP error",
                  "x" = paste0("[", obj$error$code, "] ", obj$error$message)
                ))
              }
              return(obj$result)
            }
          }
        }

        private$check_alive()
      }

      cli::cli_abort("Copilot ACP request timed out after {timeout} seconds.")
    },

    # Read NDJSON lines for session/prompt -- collects session/update
    # notifications and extracts text chunks until the final response arrives.
    # Also handles:
    # - session/request_permission: auto-responds based on permission_policy
    # - tool_call / tool_call_update events: captured in tool_calls list
    receive_prompt_response = function(id, timeout = 60) {
      deadline <- Sys.time() + timeout
      buffer <- ""
      text_parts <- character()
      thought_parts <- character()
      events <- list()
      tool_calls <- list()

      # Helper to build partial response from current state
      build_partial <- function(stop_reason = "interrupted") {
        hal_response(
          text = paste0(text_parts, collapse = ""),
          stop_reason = stop_reason,
          tool_calls = tool_calls,
          thoughts = thought_parts,
          events = events
        )
      }

      # Keep partial_response updated so interrupt handler can access it
      private$partial_response <- build_partial()

      while (Sys.time() < deadline) {
        # Check programmatic cancel
        if (private$cancelled) {
          cli::cli_inform(c("!" = "Prompt cancelled."))
          return(build_partial())
        }

        chunk <- private$read_stdout(500)
        if (nzchar(chunk)) {
          # Reset deadline on activity -- server-side tool calls can be slow
          deadline <- Sys.time() + timeout

          result <- private$parse_ndjson_chunk(buffer, chunk)
          buffer <- result$buffer

          for (obj in result$parsed) {
            # Permission request -- has an id and method
            if (identical(obj$method, "session/request_permission") &&
                !is.null(obj$id)) {
              private$handle_permission_request(obj)
              next
            }

            # Session update notification (streaming)
            if (identical(obj$method, "session/update")) {
              events <- c(events, list(obj))
              update <- obj$params$update
              update_type <- update$sessionUpdate

              if (identical(update_type, "agent_message_chunk")) {
                content <- update$content
                if (identical(content$type, "text") && !is.null(content$text)) {
                  text_parts <- c(text_parts, content$text)
                  if (is.function(private$cb_on_text)) {
                    private$cb_on_text(content$text)
                  }
                }
              } else if (identical(update_type, "agent_thought_chunk")) {
                # Model reasoning / thinking
                content <- update$content
                if (identical(content$type, "text") && !is.null(content$text)) {
                  thought_parts <- c(thought_parts, content$text)
                  if (is.function(private$cb_on_thought)) {
                    private$cb_on_thought(content$text)
                  }
                }
              } else if (identical(update_type, "tool_call")) {
                # Tool call initiated (pending)
                tc <- hal_tool_call(
                  tool_call_id = update$toolCallId,
                  title = update$title,
                  kind = update$kind,
                  status = update$status %||% "pending",
                  input = update$rawInput
                )
                tool_calls <- c(tool_calls, list(tc))
                if (is.function(private$cb_on_tool_call)) {
                  private$cb_on_tool_call(tc)
                }
              } else if (identical(update_type, "tool_call_update")) {
                # Tool call completed/failed
                tc_id <- update$toolCallId
                for (i in seq_along(tool_calls)) {
                  if (identical(tool_calls[[i]]$tool_call_id, tc_id)) {
                    tool_calls[[i]]$status <- update$status %||% "updated"
                    if (!is.null(update$rawOutput)) {
                      tool_calls[[i]]$output <- update$rawOutput$content
                    }
                    if (is.function(private$cb_on_tool_call)) {
                      private$cb_on_tool_call(tool_calls[[i]])
                    }
                    break
                  }
                }
              }

              # Update partial response after processing each event
              private$partial_response <- build_partial()
              next
            }

            # Final response with matching id
            if (!is.null(obj$id) && obj$id == id) {
              if (!is.null(obj$error)) {
                cli::cli_abort(c(
                  "Copilot ACP error",
                  "x" = paste0("[", obj$error$code, "] ", obj$error$message)
                ))
              }
              resp <- hal_response(
                text = paste0(text_parts, collapse = ""),
                stop_reason = obj$result$stopReason,
                tool_calls = tool_calls,
                thoughts = thought_parts,
                events = events
              )
              private$partial_response <- NULL
              return(resp)
            }
          }
        }

        # Check for IPC eval_r requests from MCP subprocess
        private$process_ipc()

        private$check_alive()
      }

      cli::cli_abort("Copilot ACP prompt timed out after {timeout} seconds.")
    },

    # Poll then read stdout -- processx requires poll_io() before read_output()
    read_stdout = function(poll_ms = 500) {
      private$process$poll_io(poll_ms)
      private$process$read_output()
    },

    # Handle a session/request_permission JSON-RPC request from the CLI.
    # Responds with the selected option based on permission_policy.
    handle_permission_request = function(obj) {
      req_id <- obj$id
      options <- obj$params$options %||% list()

      # Determine which option to select
      policy <- private$permission_policy
      if (is.function(policy)) {
        option_id <- policy(obj$params)
      } else if (identical(policy, "auto-deny")) {
        # Find a reject option
        option_id <- NULL
        for (opt in options) {
          if (grepl("reject|deny", opt$kind %||% "", ignore.case = TRUE)) {
            option_id <- opt$optionId
            break
          }
        }
        option_id <- option_id %||% "reject-once"
      } else {
        # Default: auto-allow -- find an allow option
        option_id <- NULL
        for (opt in options) {
          if (grepl("allow", opt$kind %||% "", ignore.case = TRUE)) {
            option_id <- opt$optionId
            break
          }
        }
        option_id <- option_id %||% "allow-once"
      }

      # Send the response
      response <- jsonlite::toJSON(
        list(
          jsonrpc = "2.0",
          id = req_id,
          result = list(
            outcome = list(
              outcome = "selected",
              optionId = option_id
            )
          )
        ),
        auto_unbox = TRUE,
        null = "null"
      )

      private$send_ndjson(response)

      # Log what was permitted (tool name + kind)
      tc <- obj$params$toolCall
      tool_name <- tc$name %||% tc$title %||% ""
      tool_kind <- obj$params$permissionRequest$kind %||%
        tc$kind %||% ""
      # "other" is uninformative (default for MCP tools), skip it
      if (identical(tool_kind, "other")) tool_kind <- ""

      # Extract the tool input (e.g., the R code being executed)
      tool_input <- tc$input %||% tc$rawInput %||% tc$arguments %||% ""
      if (is.list(tool_input)) {
        # For eval_r, input is list(code = "..."); extract the code
        tool_input <- tool_input$code %||%
          paste(names(tool_input), unlist(tool_input),
                sep = ": ", collapse = ", ")
      }
      if (is.character(tool_input) && nzchar(tool_input)) {
        # Truncate long inputs for display
        display_input <- if (nchar(tool_input) > 120) {
          paste0(substr(tool_input, 1, 117), "...")
        } else {
          tool_input
        }
      } else {
        display_input <- NULL
      }

      if (nzchar(tool_name)) {
        label <- if (nzchar(tool_kind)) {
          paste0(tool_name, " (", tool_kind, ")")
        } else {
          tool_name
        }
        if (!is.null(display_input)) {
          cli::cli_inform(c("i" = "Permission: {label} [{option_id}]",
                            " " = "  {display_input}"))
        } else {
          cli::cli_inform(c("i" = "Permission: {label} [{option_id}]"))
        }
      } else {
        cli::cli_inform(c("i" = "Permission: {option_id}"))
      }
    },

    # Process pending IPC eval_r requests from the MCP subprocess.
    # The MCP script writes request-{id}.json files; we eval the code locally
    # and write response-{id}.json files back.
    process_ipc = function() {
      if (is.null(private$ipc_dir)) return()
      if (!dir.exists(private$ipc_dir)) return()

      req_files <- list.files(
        private$ipc_dir, pattern = "^request-.*\\.json$", full.names = TRUE
      )
      for (req_file in req_files) {
        req <- .hal_safe_read_json(req_file)
        if (is.null(req) || is.null(req$id)) {
          unlink(req_file)
          next
        }
        unlink(req_file)

        kind <- req$kind %||% "eval"
        result <- if (identical(kind, "permission") &&
                      is.function(private$ipc_permission_fn)) {
          tryCatch(
            private$ipc_permission_fn(req),
            error = function(e) list(
              result = NULL,
              error = paste("IPC permission error:", conditionMessage(e))
            )
          )
        } else if (identical(kind, "eval") &&
                   is.function(private$ipc_eval_fn) &&
                   !is.null(req$code)) {
          tryCatch(
            private$ipc_eval_fn(req$code),
            error = function(e) list(
              result = NULL,
              error = paste("IPC eval error:", conditionMessage(e))
            )
          )
        } else {
          list(result = NULL,
               error = paste0("No IPC handler for kind: ", kind))
        }

        resp_list <- list(id = req$id, result = result$result,
                          error = result$error)
        # Plot vision: base64 PNG rides along as an extra field; the
        # subprocess passes unknown fields through untouched.
        if (!is.null(result$image)) resp_list$image <- result$image
        resp <- as.character(jsonlite::toJSON(
          resp_list, auto_unbox = TRUE, null = "null"
        ))
        resp_file <- file.path(private$ipc_dir,
                               paste0("response-", req$id, ".json"))
        if (!.hal_atomic_write(resp_file, resp)) {
          cli::cli_warn(c(
            "!" = "IPC response write failed for {.val {req$id}}.",
            "i" = "Likely an antivirus / EDR file lock on tempdir."
          ))
        }
      }
    },

    check_alive = function() {
      if (!private$process$is_alive()) {
        stderr_out <- private$process$read_all_error()
        cli::cli_abort(c(
          "Copilot CLI process died unexpectedly.",
          "x" = if (nzchar(stderr_out)) stderr_out else "No stderr output."
        ))
      }
    },

    # Clean up temp MCP files if the R6 object is garbage-collected
    # without an explicit $stop() call. processx handles the subprocess
    # via cleanup = TRUE, but temp files would otherwise leak.
    finalize = function() {
      for (f in c(private$mcp_tools_path, private$mcp_script_path,
                   private$mcp_config_path)) {
        if (!is.null(f) && file.exists(f)) try(unlink(f), silent = TRUE)
      }
      if (!is.null(private$ipc_dir) && dir.exists(private$ipc_dir)) {
        try(unlink(private$ipc_dir, recursive = TRUE), silent = TRUE)
      }
    }
  )
)
