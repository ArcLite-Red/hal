#' Claude Code CLI client
#'
#' @description
#' Alternative transport backend for hal that drives Anthropic's `claude` CLI
#' instead of GitHub Copilot. Unlike [HalClient], which runs Copilot CLI as a
#' long-lived ACP server, this client spawns a fresh `claude -p` subprocess
#' per turn and uses `--session-id` / `--resume` to carry conversation state
#' across calls.
#'
#' Custom tools (including `eval_r`) are wired via `--mcp-config`, reusing
#' the same MCP stdio server machinery as [HalClient]. Tool calls round-trip
#' through file-based IPC so `eval_r` executes in the user's live R session.
#'
#' @section Session model:
#' - First [prompt()] in a session: spawn with `--session-id <fresh-uuid>`.
#' - Every subsequent call: spawn with `--resume <uuid>`.
#' - Session state lives on disk in Claude Code's session store; hal only
#'   needs to remember the uuid.
#'
#' @seealso [HalClient] for the Copilot backend,
#'   `dev/claude_backend_findings.md` for the rationale.
#' @noRd
HalClientClaude <- R6::R6Class(
  "HalClientClaude",
  public = list(

    #' @description Create a new Claude Code client.
    #' @param model Model identifier. Defaults to Haiku 4.5 — cheapest
    #'   session-warm tier per Phase 0 measurements.
    #' @param cli_path Optional path to the `claude` binary.
    #' @param permission_policy How to handle tool permission requests:
    #'   `"auto-allow"` (default) passes `--permission-mode bypassPermissions`,
    #'   `"auto-deny"` uses `default` mode which denies non-interactively,
    #'   or `"default"` for Claude's normal policy. Function policies route
    #'   through the `permission_prompt` MCP bridge — Claude calls our tool
    #'   on every write/shell, and the tool round-trips back to the parent
    #'   R session via IPC to invoke the user's policy function.
    #' @param on_text Callback: `function(chunk)` for each text delta.
    #' @param on_tool_call Callback: `function(tool_call)` for tool_use blocks.
    #' @param on_thought Callback: `function(chunk)` for thinking blocks.
    #' @param quiet Suppress informational messages.
    initialize = function(model = "haiku",
                          cli_path = NULL,
                          permission_policy = "auto-allow",
                          on_text = NULL, on_tool_call = NULL,
                          on_thought = NULL, quiet = FALSE) {
      private$cli_info <- find_claude_cli(cli_path)
      private$model_id <- model
      private$quiet <- isTRUE(quiet)
      private$permission_policy <- permission_policy
      private$cb_on_text <- on_text
      private$cb_on_tool_call <- on_tool_call
      private$cb_on_thought <- on_thought
      private$started <- FALSE
      private$session_id <- NULL
      private$session_created <- FALSE
      private$cancelled <- FALSE
      private$process <- NULL
      private$partial_response <- NULL
      private$registered_tools <- list()
    },

    #' @description Locate and validate the CLI. No subprocess is spawned yet
    #'   — `claude -p` is one-shot per turn.
    start = function() {
      if (private$started) return(invisible(self))
      private$started <- TRUE
      if (!private$quiet) {
        cli::cli_inform(c(
          "v" = "Claude Code client ready.",
          "i" = "Command: {private$cli_info$command}",
          "i" = "Model: {.val {private$model_id}}"
        ))
      }
      invisible(self)
    },

    #' @description No-op for API compatibility with [HalClient]. Claude Code
    #'   has no persistent handshake — every `claude -p` call is self-contained.
    handshake = function(timeout = 15) {
      if (!private$started) self$start()
      invisible(list(agentInfo = list(name = "Claude Code", version = NA)))
    },

    #' @description Mint a fresh session uuid. The next [prompt()] call will
    #'   create the session via `--session-id`; subsequent calls use `--resume`.
    new_session = function(cwd = getwd(), timeout = 15) {
      if (!private$started) self$start()
      private$session_id <- uuid_v4()
      private$session_created <- FALSE
      if (!private$quiet) {
        cli::cli_inform(c(
          "v" = "Session prepared: {.val {private$session_id}}",
          "i" = "Model: {.val {private$model_id}}"
        ))
      }
      invisible(list(sessionId = private$session_id))
    },

    #' @description Send a prompt and collect the streamed response.
    #' @param text Prompt text.
    #' @param timeout Seconds before aborting. `NULL` (default) resolves to
    #'   `hal.prompt_timeout_cold` (default 180) on the session-creating call
    #'   and `hal.prompt_timeout` (default 60) on resumed turns -- Windows
    #'   cold spawn + AV scan + cold Claude session can exceed 60s.
    prompt = function(text, timeout = NULL) {
      if (is.null(timeout)) {
        timeout <- if (!private$session_created) {
          getOption("hal.prompt_timeout_cold", 180)
        } else {
          getOption("hal.prompt_timeout", 60)
        }
      }
      if (is.null(private$session_id)) self$new_session()

      # Build MCP config on first use if tools are registered. Paths are
      # stable across the session so subsequent turns reuse them.
      private$ensure_mcp()

      args <- c(
        "-p", text,
        "--output-format", "stream-json",
        "--include-partial-messages",
        "--verbose",
        "--model", private$model_id,
        private$permission_args()
      )

      if (length(private$registered_tools) > 0 &&
          !is.null(private$mcp_config_path)) {
        args <- c(args,
          "--mcp-config", private$mcp_config_path,
          "--strict-mcp-config"
        )
        # Pre-approve our MCP tools so bypassPermissions or default mode
        # passes them through without inline permission round-trips.
        tool_names <- paste0("mcp__r-tools__", names(private$registered_tools))
        args <- c(args, "--allowedTools", paste(tool_names, collapse = ","))
      }

      # First call: create. After: resume.
      if (!private$session_created) {
        args <- c(args, "--session-id", private$session_id)
      } else {
        args <- c(args, "--resume", private$session_id)
      }

      # Prepend prefix_args if present (mock CLI harness uses this to inject
      # `--vanilla mock_claude.R <scenario>` ahead of the real claude flags).
      prefix <- private$cli_info$prefix_args
      if (!is.null(prefix)) args <- c(prefix, args)

      private$cancelled <- FALSE
      private$process <- processx::process$new(
        command = private$cli_info$command,
        args = args,
        stdout = "|", stderr = "|",
        cleanup = TRUE, cleanup_tree = TRUE
      )

      resp <- tryCatch(
        private$receive_stream(timeout = timeout),
        interrupt = function(cnd) {
          private$cancelled <- TRUE
          if (private$process$is_alive()) private$process$kill()
          cli::cli_inform(c("!" = "Prompt interrupted."))
          private$partial_response %||% hal_response(stop_reason = "interrupted")
        }
      )

      if (!identical(resp$stop_reason, "interrupted")) {
        private$session_created <- TRUE
      }

      resp
    },

    #' @description Cancel an in-flight prompt.
    cancel = function() {
      private$cancelled <- TRUE
      if (!is.null(private$process) && private$process$is_alive()) {
        private$process$kill()
      }
      invisible(self)
    },

    #' @description Stop the client and reset session state.
    stop = function() {
      if (!is.null(private$process) && private$process$is_alive()) {
        private$process$kill()
      }
      private$process <- NULL
      private$started <- FALSE
      private$session_id <- NULL
      private$session_created <- FALSE
      # Clean up temp MCP files
      for (f in c(private$mcp_tools_path, private$mcp_script_path,
                   private$mcp_config_path)) {
        if (!is.null(f) && file.exists(f)) unlink(f)
      }
      private$mcp_tools_path <- NULL
      private$mcp_script_path <- NULL
      private$mcp_config_path <- NULL
      if (!is.null(private$ipc_dir) && dir.exists(private$ipc_dir)) {
        unlink(private$ipc_dir, recursive = TRUE)
      }
      private$ipc_dir <- NULL
      private$ipc_eval_fn <- NULL
      private$latest_rate_limit <- NULL
      invisible(self)
    },

    #' @description Return the current session uuid (or NULL).
    get_session_id = function() private$session_id,

    #' @description Return the most recent `rate_limit_info` payload from the
    #'   Claude stream, or `NULL` if none observed. Populated on every prompt.
    get_rate_limit = function() private$latest_rate_limit,

    #' @description Register MCP tools. Tools must be registered before the
    #'   first `$prompt()` call — MCP config is baked at that point and
    #'   reused across turns.
    register_tools = function(tools) {
      if (private$session_created) {
        cli::cli_warn(c(
          "!" = "Tools registered after session creation will not take effect.",
          "i" = "Register tools before the first prompt, or call {.code hal_reset()}."
        ))
      }
      for (nm in names(tools)) {
        private$registered_tools[[nm]] <- tools[[nm]]
      }
      invisible(self)
    },

    #' @description Set session mode (stub — Claude has no equivalent concept).
    set_mode = function(mode, timeout = 10) {
      if (!identical(mode, "agent") && !isTRUE(private$mode_warned)) {
        cli::cli_warn(c(
          "!" = "Session modes (plan/autopilot) not available on Claude backend.",
          "i" = "Claude Code has plan mode via its own CLI, not exposed through hal yet."
        ))
        private$mode_warned <- TRUE
      }
      invisible(self)
    },

    #' @description Switch model mid-session (stub — Claude doesn't support this).
    switch_model = function(model, timeout = 10) {
      cli::cli_warn(c(
        "!" = "Mid-session model switching not available on Claude backend.",
        "i" = "Start a new session with {.code hal_reset()} and a different default model."
      ))
      invisible(self)
    },

    #' @description Swap to a fresh session, returning the previous uuid.
    swap_session = function() {
      saved <- private$session_id
      private$session_id <- NULL
      private$session_created <- FALSE
      self$new_session()
      invisible(saved)
    },

    #' @description Restore a previously saved session uuid.
    restore_session = function(session_id) {
      private$session_id <- session_id
      private$session_created <- !is.null(session_id)
      invisible(self)
    },

    #' @description Configure IPC for live eval_r execution.
    #'
    #' Stores the IPC directory and eval function. The directory is passed to
    #' the MCP subprocess via `build_mcp_config()` on the first prompt. During
    #' streaming, the response loop polls the directory for eval requests and
    #' executes them in the user's session.
    set_ipc = function(ipc_dir, eval_fn = NULL, permission_fn = NULL) {
      private$ipc_dir <- ipc_dir
      private$ipc_eval_fn <- eval_fn
      private$ipc_permission_fn <- permission_fn
      invisible(self)
    },

    #' @description True if a subprocess is currently running (only during a
    #'   prompt call — claude -p exits between turns).
    is_alive = function() {
      !is.null(private$process) && private$process$is_alive()
    },

    #' @description Read any available stderr output.
    read_stderr = function() {
      if (!is.null(private$process)) private$process$read_error() else ""
    }
  ),

  private = list(
    cli_info = NULL,
    model_id = NULL,
    quiet = FALSE,
    permission_policy = "auto-allow",
    started = FALSE,
    session_id = NULL,
    session_created = FALSE,
    cancelled = FALSE,
    process = NULL,
    partial_response = NULL,
    cb_on_text = NULL,
    cb_on_tool_call = NULL,
    cb_on_thought = NULL,
    mode_warned = FALSE,

    registered_tools = list(),
    mcp_config_path = NULL,
    mcp_script_path = NULL,
    mcp_tools_path = NULL,
    ipc_dir = NULL,
    ipc_eval_fn = NULL,
    ipc_permission_fn = NULL,
    latest_rate_limit = NULL,

    # Build MCP config once per session if tools are registered. Idempotent.
    ensure_mcp = function() {
      if (length(private$registered_tools) == 0) return(invisible())
      if (!is.null(private$mcp_config_path)) return(invisible())

      mcp <- build_mcp_config(
        private$registered_tools,
        ipc_dir = private$ipc_dir,
        backend = "claude"
      )
      private$mcp_config_path <- mcp$config_path
      private$mcp_script_path <- mcp$script_path
      private$mcp_tools_path  <- mcp$tools_path

      if (!private$quiet) {
        cli::cli_inform(c(
          "i" = "MCP config: {length(private$registered_tools)} tool{?s} via --mcp-config"
        ))
      }
      invisible()
    },

    # Map hal.permission_policy to `--permission-mode` (and, for function
    # policies, `--permission-prompt-tool` so Claude routes approval requests
    # back into the parent R session via our MCP `permission_prompt` tool).
    permission_args = function() {
      p <- private$permission_policy
      if (is.function(p)) {
        # Tool name is namespaced by the MCP server name `r-tools`.
        return(c(
          "--permission-prompt-tool", "mcp__r-tools__permission_prompt",
          "--permission-mode", "default"
        ))
      }
      mode <- switch(p,
        `auto-allow` = "bypassPermissions",
        `auto-deny`  = "default",
        p  # pass through raw values like "acceptEdits", "plan"
      )
      c("--permission-mode", mode)
    },

    # Read NDJSON from stdout until the `result` event (or timeout/cancel).
    # Builds a hal_response by mapping Claude's stream-json to hal's
    # internal event vocabulary. See CLAUDE.md "Alternative Backend" for
    # the mapping table.
    receive_stream = function(timeout = 60) {
      deadline <- Sys.time() + timeout
      buffer <- ""
      text_parts <- character()
      thought_parts <- character()
      tool_calls <- list()
      events <- list()
      stop_reason <- NULL

      build_partial <- function(reason = "interrupted") {
        hal_response(
          text = paste0(text_parts, collapse = ""),
          stop_reason = reason,
          tool_calls = tool_calls,
          thoughts = thought_parts,
          events = events
        )
      }
      private$partial_response <- build_partial()

      proc <- private$process

      while (Sys.time() < deadline) {
        if (private$cancelled) return(build_partial("interrupted"))

        proc$poll_io(500)
        chunk <- proc$read_output()

        if (nzchar(chunk)) {
          deadline <- Sys.time() + timeout
          parsed <- private$parse_ndjson_chunk(buffer, chunk)
          buffer <- parsed$buffer
          for (obj in parsed$parsed) {
            private$dispatch_event(obj,
              on_text_chunk    = function(t) text_parts    <<- c(text_parts, t),
              on_thought_chunk = function(t) thought_parts <<- c(thought_parts, t),
              on_tool_call     = function(tc) tool_calls   <<- c(tool_calls, list(tc)),
              on_tool_result   = function(id, blk) {
                for (i in seq_along(tool_calls)) {
                  if (identical(tool_calls[[i]]$tool_call_id, id)) {
                    tool_calls[[i]]$status <- if (isTRUE(blk$is_error)) "failed" else "completed"
                    tool_calls[[i]]$result <- blk$content
                    tool_calls <<- tool_calls
                    break
                  }
                }
              },
              on_result        = function(r) stop_reason   <<- r
            )
            events <- c(events, list(obj))
            private$partial_response <- build_partial(stop_reason %||% "interrupted")
            if (!is.null(stop_reason)) break
          }
          if (!is.null(stop_reason)) break
        }

        # Check for eval_r requests from MCP subprocess and respond inline.
        # These run the user's R code synchronously in this loop, so the time
        # they take is hal working, not Claude stalling -- credit it back to
        # the deadline. Without this a slow eval_r (eval_timeout defaults to
        # 30s, roughly double that when plot vision re-renders) can burn the
        # whole prompt budget and abort a turn that was proceeding normally.
        ipc_start <- Sys.time()
        n_ipc <- private$process_ipc()
        if (isTRUE(n_ipc > 0L)) {
          deadline <- deadline +
            as.numeric(difftime(Sys.time(), ipc_start, units = "secs"))
        }

        if (!proc$is_alive() && !nzchar(chunk)) {
          # Drain anything remaining on the pipe
          leftover <- proc$read_output()
          if (nzchar(leftover)) {
            parsed <- private$parse_ndjson_chunk(buffer, leftover)
            buffer <- parsed$buffer
            for (obj in parsed$parsed) {
              private$dispatch_event(obj,
                on_text_chunk    = function(t) text_parts    <<- c(text_parts, t),
                on_thought_chunk = function(t) thought_parts <<- c(thought_parts, t),
                on_tool_call     = function(tc) tool_calls   <<- c(tool_calls, list(tc)),
                on_tool_result   = function(id, blk) {
                  for (i in seq_along(tool_calls)) {
                    if (identical(tool_calls[[i]]$tool_call_id, id)) {
                      tool_calls[[i]]$status <- if (isTRUE(blk$is_error)) "failed" else "completed"
                      tool_calls[[i]]$result <- blk$content
                      tool_calls <<- tool_calls
                      break
                    }
                  }
                },
                on_result        = function(r) stop_reason   <<- r
              )
              events <- c(events, list(obj))
            }
          }
          break
        }
      }

      if (is.null(stop_reason)) {
        stderr_out <- tryCatch(proc$read_error(), error = function(e) "")
        stdout_tail <- tryCatch(proc$read_output(), error = function(e) "")
        alive <- proc$is_alive()
        exit_status <- if (!alive) proc$get_exit_status() else NA_integer_

        # Dump raw streams to a temp file so we can inspect them regardless
        # of how cli renders the error. The path is printed in the abort
        # message and is also stashed on the process for later access.
        dump_path <- tempfile("hal_claude_fail_", fileext = ".log")
        dump_lines <- c(
          paste0("# Claude CLI failure at ", format(Sys.time())),
          paste0("# exit_status: ", exit_status, "  alive: ", alive,
                 "  timeout: ", timeout, "s"),
          "",
          "## STDERR ##",
          if (nzchar(stderr_out)) stderr_out else "(empty)",
          "",
          "## STDOUT (tail) ##",
          if (nzchar(stdout_tail)) stdout_tail else "(empty)"
        )
        tryCatch(
          writeLines(dump_lines, dump_path),
          error = function(e) NULL
        )

        if (!alive) {
          cat("\n--- Claude CLI early-exit diagnostics ---\n")
          cat("exit_status:", exit_status, "\n")
          cat("stderr:\n", if (nzchar(stderr_out)) stderr_out else "(empty)",
              "\n", sep = "")
          cat("stdout tail:\n",
              if (nzchar(stdout_tail)) substr(stdout_tail, 1, 800) else "(empty)",
              "\n", sep = "")
          cat("full log written to:", dump_path, "\n")
          cat("-----------------------------------------\n\n")
          cli::cli_abort(c(
            paste0("Claude CLI exited without producing a result event ",
                   "(status ", exit_status, ")."),
            "i" = "See diagnostics above; full log at {.path {dump_path}}",
            "i" = "If this repeats, {.code hal_reset()} starts a clean session."
          ))
        }
        cli::cli_abort(c(
          paste0("Claude CLI prompt timed out after ", timeout, " seconds."),
          "i" = "Diagnostics at {.path {dump_path}}",
          "i" = paste("The session is still resumable -- retry the prompt, or",
                      "{.code hal_reset()} to start fresh.")
        ))
      }

      resp <- hal_response(
        text = paste0(text_parts, collapse = ""),
        stop_reason = stop_reason,
        tool_calls = tool_calls,
        thoughts = thought_parts,
        events = events
      )
      private$partial_response <- NULL
      resp
    },

    # Map one Claude stream-json event to hal callbacks.
    dispatch_event = function(obj,
                              on_text_chunk, on_thought_chunk,
                              on_tool_call, on_tool_result, on_result) {
      type <- obj$type %||% ""

      # Any event carrying a session_id means the CLI accepted --session-id
      # and the session now exists in Claude Code's store. Latch it here
      # rather than after a successful turn: if this turn later times out or
      # is interrupted, the next prompt must --resume: re-sending
      # --session-id with a live uuid is rejected, which would wedge the
      # client until hal_reset(). A CLI that dies before emitting any event
      # never created the session, so the flag correctly stays FALSE.
      if (!is.null(obj$session_id)) private$session_created <- TRUE

      if (identical(type, "stream_event")) {
        ev <- obj$event
        if (identical(ev$type, "content_block_delta")) {
          d <- ev$delta
          if (identical(d$type, "text_delta") && nzchar(d$text %||% "")) {
            on_text_chunk(d$text)
            if (is.function(private$cb_on_text)) private$cb_on_text(d$text)
          } else if (identical(d$type, "thinking_delta") && nzchar(d$thinking %||% "")) {
            on_thought_chunk(d$thinking)
            if (is.function(private$cb_on_thought)) private$cb_on_thought(d$thinking)
          }
        }
        return(invisible())
      }

      if (identical(type, "assistant")) {
        msg <- obj$message
        content <- msg$content %||% list()
        for (block in content) {
          btype <- block$type %||% ""
          if (identical(btype, "tool_use")) {
            tc <- hal_tool_call(
              tool_call_id = block$id,
              title = block$name,
              kind = "other",
              status = "pending",
              input = block$input
            )
            on_tool_call(tc)
            if (is.function(private$cb_on_tool_call)) private$cb_on_tool_call(tc)
          }
        }
        return(invisible())
      }

      if (identical(type, "user")) {
        # tool_result blocks: thread status + result back to the matching
        # tool_use entry by tool_use_id so $tool_calls reflects completion.
        msg <- obj$message
        content <- msg$content %||% list()
        for (block in content) {
          if (identical(block$type %||% "", "tool_result")) {
            on_tool_result(block$tool_use_id, block)
          }
        }
        return(invisible())
      }

      if (identical(type, "rate_limit_event")) {
        rl <- obj$rate_limit_info
        if (!is.null(rl)) private$latest_rate_limit <- rl
        return(invisible())
      }

      if (identical(type, "result")) {
        on_result(obj$stop_reason %||% obj$subtype %||% "end_turn")
        return(invisible())
      }

      # system/init and other unmapped types — stored in events list only
      invisible()
    },

    # Process pending IPC requests from the MCP subprocess.
    # Dispatches by req$kind ("eval" | "permission"). Shares the protocol
    # with HalClient::process_ipc so any future refactor can lift this to
    # a shared helper.
    # Returns the number of requests actually handled, so the caller can
    # credit the elapsed time back to the prompt deadline.
    process_ipc = function() {
      if (is.null(private$ipc_dir)) return(0L)
      if (!dir.exists(private$ipc_dir)) return(0L)

      req_files <- list.files(
        private$ipc_dir, pattern = "^request-.*\\.json$", full.names = TRUE
      )
      handled <- 0L
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
        handled <- handled + 1L
      }
      handled
    },

    parse_ndjson_chunk = function(buffer, chunk) {
      buffer <- paste0(buffer, chunk)
      lines <- strsplit(buffer, "\n", fixed = TRUE)[[1]]
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
        if (!is.null(obj)) parsed <- c(parsed, list(obj))
      }
      list(parsed = parsed, buffer = remaining)
    },

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

# --- helpers ----------------------------------------------------------------

# RFC 4122 v4 uuid — 8-4-4-4-12 lowercase hex, version nibble = 4,
# variant bits = 10xx. Good enough for --session-id; no crypto properties
# needed beyond uniqueness within a session store.
uuid_v4 <- function() {
  # sample() consumes user RNG state; preserve it so set.seed() in user code
  # doesn't make session ids deterministic (collisions across runs) and so
  # reproducible workflows don't see their RNG stream perturbed by hal.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    saved <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", saved, envir = globalenv()), add = TRUE)
  } else {
    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
  }
  # Reseed from an entropy source that is independent of user state.
  set.seed(NULL)
  bytes <- as.raw(sample(0:255, 16, replace = TRUE))
  bytes[7] <- as.raw(bitwOr(bitwAnd(as.integer(bytes[7]), 0x0F), 0x40))
  bytes[9] <- as.raw(bitwOr(bitwAnd(as.integer(bytes[9]), 0x3F), 0x80))
  hex <- paste(sprintf("%02x", as.integer(bytes)), collapse = "")
  paste(
    substr(hex, 1, 8),
    substr(hex, 9, 12),
    substr(hex, 13, 16),
    substr(hex, 17, 20),
    substr(hex, 21, 32),
    sep = "-"
  )
}

find_claude_cli <- function(cli_path = NULL) {
  if (!is.null(cli_path)) {
    if (!file.exists(cli_path)) {
      cli::cli_abort(c(
        "Claude CLI not found at specified path.",
        "x" = "File does not exist: {.path {cli_path}}"
      ))
    }
    return(list(command = .hal_prefer_native_claude(cli_path)))
  }

  env_path <- Sys.getenv("CLAUDE_CLI_PATH", "")
  if (nzchar(env_path) && file.exists(env_path)) {
    return(list(command = .hal_prefer_native_claude(env_path)))
  }

  claude_path <- as.character(Sys.which("claude"))
  if (nzchar(claude_path)) {
    return(list(command = .hal_prefer_native_claude(claude_path)))
  }

  cli::cli_abort(c(
    "Could not find the Claude Code CLI.",
    "i" = "Install Claude Code from {.url https://claude.ai/download}.",
    "i" = "Or set {.envvar CLAUDE_CLI_PATH} to the binary path."
  ))
}

# On Windows, `Sys.which("claude")` resolves to a .cmd / .ps1 npm shim. When
# spawned via processx the shim's stdio is bridged through cmd.exe and the
# child .exe's stdout/stderr can get silently dropped on the floor -- the
# observed symptom is exit_status 0 with empty pipes even though the real
# claude session was provisioned. Resolve to the underlying .exe instead.
.hal_prefer_native_claude <- function(path) {
  if (.Platform$OS.type != "windows") return(path)
  if (grepl("\\.exe$", path, ignore.case = TRUE)) return(path)

  shim_dir <- dirname(path)
  candidates <- c(
    file.path(shim_dir, "node_modules", "@anthropic-ai", "claude-code",
              "bin", "claude.exe"),
    file.path(shim_dir, "claude.exe")
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(normalizePath(cand, winslash = "\\"))
  }
  path
}

#' Check if the Claude Code CLI is available
#' @return Logical.
#' @noRd
claude_available <- function() {
  tryCatch({
    info <- find_claude_cli()
    result <- processx::run(info$command, "--version",
                            timeout = 10, error_on_status = FALSE)
    result$status == 0L && grepl("[0-9]", result$stdout)
  }, error = function(e) FALSE)
}
