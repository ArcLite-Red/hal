# ==============================================================================
# HalClientVSCode -- transport for the hal-bridge Positron/VS Code extension
# ==============================================================================
#
# Unlike the Copilot and Claude backends, this client never spawns a CLI
# subprocess. The hal-bridge VS Code extension runs an HTTP server on
# 127.0.0.1; we discover its port via a file in a durable per-user app-data
# dir (see .hal_bridge_port_file), then POST chat requests and stream
# Server-Sent Events back.
#
# Conversation state lives entirely on the R side -- the bridge is stateless,
# so we resend the full message history on every turn. Tool calls execute
# locally in R (no MCP, no IPC), then round-trip via a second POST with the
# tool result attached.
#
# Bridge discovery, install, and status live in R/bridge.R.

#' Claude / Copilot-compatible transport client speaking to hal-bridge.
#' @noRd
HalClientVSCode <- R6::R6Class(
  "HalClientVSCode",
  public = list(
    initialize = function(model = NULL, cli_path = NULL,
                          permission_policy = "auto-allow",
                          on_text = NULL, on_tool_call = NULL,
                          on_thought = NULL, quiet = FALSE) {
      # `cli_path` accepted for API parity; ignored (no CLI here).
      private$model_id <- model
      private$permission_policy <- permission_policy
      private$cb_on_text <- on_text
      private$cb_on_tool_call <- on_tool_call
      private$cb_on_thought <- on_thought
      private$quiet <- isTRUE(quiet)
      private$started <- FALSE
      private$session_id <- NULL
      private$cancelled <- FALSE
      private$registered_tools <- list()
      private$messages <- list()
      private$saved_sessions <- list()
    },

    start = function() {
      if (private$started) return(invisible(self))
      info <- .hal_bridge_discover()
      private$bridge_info <- info
      private$started <- TRUE
      if (!private$quiet) {
        cli::cli_inform(c(
          "v" = "hal-bridge ready (v{info$version} on port {info$port}).",
          "i" = "Model: {.val {private$model_id %||% '(default)'}}"
        ))
      }
      invisible(self)
    },

    stop = function() {
      private$started <- FALSE
      private$session_id <- NULL
      private$messages <- list()
      invisible(self)
    },

    handshake = function(timeout = 15) {
      if (!private$started) self$start()
      invisible(list(agentInfo = list(name = "hal-bridge",
                                       version = private$bridge_info$version)))
    },

    new_session = function(cwd = getwd(), timeout = 15) {
      if (!private$started) self$start()
      private$session_id <- uuid_v4()
      private$messages <- list()
      if (!private$quiet) {
        cli::cli_inform(c("v" = "Session created: {.val {private$session_id}}"))
      }
      invisible(list(sessionId = private$session_id))
    },

    register_tools = function(tools) {
      for (nm in names(tools)) {
        private$registered_tools[[nm]] <- tools[[nm]]
      }
      invisible(self)
    },

    switch_model = function(model, timeout = 10) {
      private$model_id <- model
      if (!private$quiet) cli::cli_inform(c("v" = "Model set to {.val {model}}."))
      invisible(self)
    },

    set_mode = function(mode = c("agent", "plan", "autopilot"), timeout = 10) {
      # No-op: vscode.lm does not expose ACP session modes.
      mode <- match.arg(mode)
      if (!identical(mode, "agent") && !private$quiet) {
        cli::cli_warn(c(
          "!" = "Mode {.val {mode}} is not supported on the vscode backend.",
          "i" = "Treating as {.val agent}."
        ))
      }
      invisible(self)
    },

    get_session_id = function() private$session_id,

    swap_session = function() {
      saved <- list(id = private$session_id, messages = private$messages)
      private$saved_sessions <- c(private$saved_sessions, list(saved))
      private$session_id <- NULL
      private$messages <- list()
      self$new_session()
      invisible(saved$id)
    },

    restore_session = function(session_id) {
      if (length(private$saved_sessions) == 0L) {
        private$session_id <- session_id
        return(invisible(self))
      }
      last <- private$saved_sessions[[length(private$saved_sessions)]]
      private$saved_sessions[[length(private$saved_sessions)]] <- NULL
      private$session_id <- last$id %||% session_id
      private$messages <- last$messages %||% list()
      invisible(self)
    },

    set_ipc = function(ipc_dir, eval_fn = NULL, permission_fn = NULL) {
      # No-op: the vscode backend executes tools directly in-process.
      invisible(self)
    },

    cancel = function() {
      private$cancelled <- TRUE
      invisible(self)
    },

    is_alive = function() private$started,

    read_stderr = function() "",

    prompt = function(text, timeout = NULL) {
      if (!private$started) self$start()
      if (is.null(private$session_id)) self$new_session()
      if (is.null(timeout)) timeout <- getOption("hal.prompt_timeout", 60)
      private$cancelled <- FALSE

      private$messages <- c(private$messages, list(
        list(role = "user", content = text)
      ))

      tool_defs <- private$tool_payload()
      text_parts <- character()
      tool_calls <- list()
      events <- list()
      stop_reason <- "end_turn"
      max_rounds <- getOption("hal.vscode_max_tool_rounds", 8)

      for (round in seq_len(max_rounds)) {
        if (private$cancelled) {
          stop_reason <- "interrupted"
          break
        }

        round_calls <- list()
        on_event <- function(ev) {
          events[[length(events) + 1L]] <<- ev
          if (identical(ev$type, "text")) {
            text_parts <<- c(text_parts, ev$value %||% "")
            if (is.function(private$cb_on_text)) {
              private$cb_on_text(ev$value %||% "")
            }
          } else if (identical(ev$type, "tool_call")) {
            tc <- hal_tool_call(
              tool_call_id = ev$callId,
              title = ev$name,
              kind = "execute",
              status = "pending",
              input = ev$input
            )
            round_calls[[length(round_calls) + 1L]] <<- list(
              call = tc, name = ev$name, input = ev$input, id = ev$callId
            )
            tool_calls[[length(tool_calls) + 1L]] <<- tc
            if (is.function(private$cb_on_tool_call)) {
              private$cb_on_tool_call(tc)
            }
          } else if (identical(ev$type, "error")) {
            cli::cli_abort(c(
              "hal-bridge error", "x" = ev$message %||% "(unknown)"
            ))
          }
        }

        # Cap image payloads in the resend history before serializing
        private$prune_images()

        body <- jsonlite::toJSON(list(
          id = private$model_id,
          messages = private$messages,
          tools = tool_defs
        ), auto_unbox = TRUE, null = "null")

        .hal_bridge_stream(private$bridge_info$port, "/chat", body,
                           on_event = on_event,
                           token = private$bridge_info$token,
                           timeout = timeout)

        if (length(round_calls) == 0L) break

        # Build assistant turn from this round, then user turn with results.
        asst_content <- list()
        round_text <- paste0(text_parts[max(1L, length(text_parts) -
                              length(round_calls) + 1L):length(text_parts)],
                              collapse = "")
        if (nzchar(round_text)) {
          asst_content <- c(asst_content, list(list(
            type = "text", value = round_text
          )))
        }
        for (rc in round_calls) {
          asst_content <- c(asst_content, list(list(
            type = "tool_call", callId = rc$id, name = rc$name, input = rc$input
          )))
        }
        private$messages <- c(private$messages, list(list(
          role = "assistant", content = asst_content
        )))

        # Execute tool calls and append tool_result(s) as a user turn.
        result_content <- list()
        for (rc in round_calls) {
          decision <- private$authorize(rc$name, rc$input)
          res <- if (isTRUE(decision$allow)) {
            private$execute_tool(rc$name, rc$input)
          } else {
            list(text = paste0(
              "Error: tool call denied by permission_policy",
              if (nzchar(decision$reason %||% "")) {
                paste0(" (", decision$reason, ")")
              } else "",
              "."
            ), image = NULL)
          }
          result_text <- res$text
          final_status <- if (isTRUE(decision$allow)) "completed" else "denied"
          for (i in seq_along(tool_calls)) {
            if (identical(tool_calls[[i]]$tool_call_id, rc$id)) {
              tool_calls[[i]]$status <- final_status
              tool_calls[[i]]$output <- result_text
              if (is.function(private$cb_on_tool_call)) {
                private$cb_on_tool_call(tool_calls[[i]])
              }
              break
            }
          }
          # Build the wire part; `image` only present when non-NULL so the
          # serialized JSON never carries "image": null (old bridges ignore
          # unknown fields, so this is backward compatible).
          part <- list(type = "tool_result", callId = rc$id,
                       content = result_text)
          if (!is.null(res$image)) part$image <- res$image
          result_content <- c(result_content, list(part))
        }
        private$messages <- c(private$messages, list(list(
          role = "user", content = result_content
        )))
      }

      # Finalize: append final assistant text turn if non-empty.
      final_text <- paste0(text_parts, collapse = "")
      private$messages <- c(private$messages, list(list(
        role = "assistant", content = final_text
      )))

      hal_response(
        text = final_text,
        stop_reason = stop_reason,
        tool_calls = tool_calls,
        thoughts = character(),
        events = events
      )
    }
  ),

  private = list(
    model_id = NULL,
    permission_policy = "auto-allow",
    cb_on_text = NULL,
    cb_on_tool_call = NULL,
    cb_on_thought = NULL,
    quiet = FALSE,
    started = FALSE,
    bridge_info = NULL,
    session_id = NULL,
    messages = list(),
    saved_sessions = list(),
    cancelled = FALSE,
    registered_tools = list(),

    tool_payload = function() {
      lapply(private$registered_tools, function(t) {
        # Tools registered via as_tool_def() carry their JSON Schema at
        # t$schema$function$parameters; eval_r and other hand-built defs put it
        # at the top level as t$parameters. Read both so neither is sent to the
        # model with empty parameters.
        params <- t$parameters %||%
          t$schema[["function"]]$parameters %||%
          list(type = "object",
               properties = structure(list(), names = character()))
        list(
          name = t$name,
          description = t$description %||% "",
          parameters = params
        )
      }) |> unname()
    },

    # Consult permission_policy before executing a tool call.
    #
    # Returns list(allow = logical, reason = character). The vscode bridge has
    # no analog of ACP `session/request_permission`, so this is the *only*
    # gate between a model-emitted tool call and live execution in the user's
    # R session. (For `eval_r` the AST denylist in governance.R still applies
    # inside the tool itself, but custom tools have no other gate.)
    #
    # Policy values:
    #   "auto-allow"           -> always allow (current default for parity)
    #   "auto-deny"            -> always deny
    #   "ask"                  -> interactive prompt; non-interactive sessions
    #                              fall back to deny (safer than auto-allow
    #                              when stdin isn't attached)
    #   function(params)       -> called with list(backend="vscode", tool_name,
    #                              input); return value parsed for "allow" vs
    #                              "deny" (case-insensitive substring match,
    #                              matching the Claude-backend contract).
    authorize = function(name, input) {
      policy <- private$permission_policy

      if (is.function(policy)) {
        params <- list(backend = "vscode", tool_name = name,
                       input = if (is.list(input)) input else list())
        decision <- tryCatch(policy(params), error = function(e) {
          cli::cli_warn(c(
            "!" = "permission_policy function errored: {conditionMessage(e)}",
            "i" = "Denying tool call as a safety default."
          ))
          "deny"
        })
        if (!is.character(decision) || length(decision) != 1L) {
          decision <- "deny"
        }
        if (grepl("allow", decision, ignore.case = TRUE)) {
          return(list(allow = TRUE, reason = ""))
        }
        return(list(allow = FALSE, reason = "policy function returned deny"))
      }

      if (identical(policy, "auto-deny")) {
        return(list(allow = FALSE, reason = "auto-deny"))
      }

      if (identical(policy, "ask")) {
        if (!interactive()) {
          return(list(allow = FALSE,
                      reason = "policy=ask in non-interactive session"))
        }
        preview <- if (is.list(input)) {
          code <- input$code %||% ""
          if (!nzchar(code)) {
            paste(names(input), unlist(lapply(input, function(x) {
              if (is.null(x)) "NULL" else paste0(utils::head(as.character(x),
                                                              1L))
            })), sep = ": ", collapse = ", ")
          } else {
            code
          }
        } else as.character(input %||% "")
        if (is.character(preview) && nchar(preview) > 200L) {
          preview <- paste0(substr(preview, 1L, 197L), "...")
        }
        cli::cli_inform(c(
          "i" = "Tool call requested: {.val {name}}",
          if (nzchar(preview)) c(" " = "  {preview}") else NULL
        ))
        ans <- tolower(trimws(readline("Allow this tool call? [y/N]: ")))
        if (ans %in% c("y", "yes")) {
          return(list(allow = TRUE, reason = ""))
        }
        return(list(allow = FALSE, reason = "user denied at prompt"))
      }

      # Default: auto-allow (including unknown string policies, for parity
      # with the other clients which also default-open).
      list(allow = TRUE, reason = "")
    },

    # Execute a registered tool. Always returns list(text, image) so the
    # round loop has one shape to handle; `image` is non-NULL only when the
    # tool returned a `hal_tool_result` (currently eval_r with plot vision).
    execute_tool = function(name, input) {
      tool <- private$registered_tools[[name]]
      if (is.null(tool) || !is.function(tool$fun)) {
        return(list(
          text = paste0("Error: tool '", name, "' is not registered."),
          image = NULL
        ))
      }
      args <- if (is.list(input)) input else list()
      tryCatch({
        out <- do.call(tool$fun, args)
        if (inherits(out, "hal_tool_result")) {
          list(text = out$text %||% "", image = out$image)
        } else {
          list(text = tool_result_text(out), image = NULL)
        }
      }, error = function(e) {
        list(text = paste("Error:", conditionMessage(e)), image = NULL)
      })
    },

    # Keep only the newest `keep` images in the resend history. The bridge
    # is stateless -- full history goes out on every round and every later
    # turn -- so unpruned images get re-billed forever. The text part of
    # older tool results is untouched.
    prune_images = function(keep = 2L) {
      carrying <- list()  # list of (msg index, part index)
      for (mi in seq_along(private$messages)) {
        content <- private$messages[[mi]]$content
        if (!is.list(content)) next
        for (pi in seq_along(content)) {
          part <- content[[pi]]
          if (is.list(part) && identical(part$type, "tool_result") &&
              !is.null(part$image)) {
            carrying <- c(carrying, list(c(mi, pi)))
          }
        }
      }
      n_drop <- length(carrying) - keep
      if (n_drop <= 0L) return(invisible(NULL))
      for (idx in carrying[seq_len(n_drop)]) {
        private$messages[[idx[1]]]$content[[idx[2]]]$image <- NULL
      }
      invisible(NULL)
    }
  )
)

# -----------------------------------------------------------------------------
# Streaming HTTP client (curl-backed SSE)
# -----------------------------------------------------------------------------

#' POST a JSON body to the bridge and dispatch SSE events.
#'
#' Uses curl::curl_fetch_stream so on_event fires as data arrives, not at the
#' end of the response. The buffer protocol is the standard SSE form:
#' `data: <json>\n\n`. Empty keep-alive lines are tolerated.
#'
#' Bridge >=0.1.1 requires `Authorization: Bearer <token>` on /chat; older
#' bridges ignore extra headers, so sending the header unconditionally is
#' safe for forward and backward compatibility.
#'
#' @param port Bridge port.
#' @param path Request path (e.g. `/chat`).
#' @param body Request body (JSON string).
#' @param on_event Callback receiving each parsed event as a list.
#' @param token Bridge bearer token from the port file (NULL skips the header).
#' @param timeout Per-request timeout in seconds.
#' @keywords internal
#' @noRd
.hal_bridge_stream <- function(port, path, body, on_event,
                                token = NULL, timeout = 60) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg curl} is required for the vscode backend.",
      "i" = "Install with {.code install.packages('curl')}."
    ))
  }
  url <- sprintf("http://127.0.0.1:%d%s", port, path)
  h <- curl::new_handle()
  headers <- c("Content-Type" = "application/json",
               "Accept" = "text/event-stream")
  if (!is.null(token) && nzchar(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }
  do.call(curl::handle_setheaders, c(list(handle = h), as.list(headers)))
  curl::handle_setopt(h,
                      customrequest = "POST",
                      postfields = body,
                      connecttimeout = 5L,
                      timeout = as.integer(timeout))

  # Surface HTTP error statuses up front. curl_fetch_stream itself doesn't
  # throw on 4xx/5xx (the body is streamed regardless), but we want a
  # directive error message for the 401/429 cases.
  status <- NULL
  buffer <- ""
  curl::curl_fetch_stream(url, function(data) {
    buffer <<- paste0(buffer, rawToChar(data))
    repeat {
      idx <- regexpr("\n\n", buffer, fixed = TRUE)
      if (idx[1] == -1L) break
      event_block <- substr(buffer, 1L, idx[1] - 1L)
      buffer <<- substr(buffer, idx[1] + 2L, nchar(buffer))
      for (line in strsplit(event_block, "\n", fixed = TRUE)[[1]]) {
        if (startsWith(line, "data: ")) {
          json_str <- substr(line, 7L, nchar(line))
          parsed <- tryCatch(
            jsonlite::fromJSON(json_str, simplifyVector = FALSE),
            error = function(e) NULL
          )
          if (!is.null(parsed)) on_event(parsed)
        }
      }
    }
  }, handle = h)

  # After the stream finishes, check the HTTP status. For SSE the bridge sets
  # 200 before streaming; non-200 means our request was rejected before the
  # stream started (auth, rate-limit, or 4xx route).
  info <- curl::handle_data(h)
  if (!is.null(info$status_code) && info$status_code >= 400L) {
    msg <- switch(as.character(info$status_code),
      "401" = "Bridge rejected our token. Reload Positron to rotate token + restart bridge, then retry.",
      "429" = "Bridge is at its concurrency limit. Wait a moment and retry.",
      paste0("Bridge returned HTTP ", info$status_code, ".")
    )
    cli::cli_abort(c("hal-bridge HTTP {info$status_code}", "x" = msg))
  }
}
