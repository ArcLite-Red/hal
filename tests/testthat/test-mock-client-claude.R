# Mock CLI tests for the Claude backend.
#
# Exercises HalClientClaude end-to-end against inst/mock-cli/mock_claude.R,
# which emits the same stream-json NDJSON shape as the real `claude -p`
# subprocess. No network, no login, no cost.

test_that("mock_claude_client construction + handshake works", {
  client <- mock_claude_client("basic")
  expect_s3_class(client, "HalClientClaude")
  expect_silent(client$start())
  expect_silent(client$handshake())
})

test_that("basic scenario returns text response + end_turn", {
  client <- mock_claude_client("basic")
  resp <- client$prompt("hello")
  expect_s3_class(resp, "hal_response")
  expect_match(resp$text, "MockClaude", fixed = TRUE)
  expect_identical(resp$stop_reason, "end_turn")
})

test_that("echo scenario round-trips prompt text", {
  client <- mock_claude_client("echo")
  resp <- client$prompt("ping-pong")
  expect_identical(resp$text, "ping-pong")
})

test_that("thinking scenario captures thinking_delta chunks", {
  thoughts <- character()
  client <- mock_claude_client(
    "thinking",
    on_thought = function(chunk) thoughts <<- c(thoughts, chunk)
  )
  resp <- client$prompt("ponder")
  expect_true(length(resp$thoughts) >= 1L)
  expect_true(length(thoughts) >= 1L)
  expect_match(paste(resp$thoughts, collapse = ""), "think", ignore.case = TRUE)
  expect_match(resp$text, "Done thinking", fixed = TRUE)
})

test_that("tool_use scenario records tool_call on response", {
  calls <- list()
  client <- mock_claude_client(
    "tool_use",
    on_tool_call = function(tc) calls[[length(calls) + 1L]] <<- tc
  )
  resp <- client$prompt("use a tool")
  expect_true(length(resp$tool_calls) >= 1L)
  expect_true(length(calls) >= 1L)
  expect_identical(calls[[1]]$title, "view_file")
})

test_that("multi_turn scenario preserves session via --resume", {
  client <- mock_claude_client("multi_turn")
  r1 <- client$prompt("turn one")
  r2 <- client$prompt("turn two")
  r3 <- client$prompt("turn three")
  expect_match(r1$text, "Turn 1", fixed = TRUE)
  expect_match(r2$text, "Turn 2", fixed = TRUE)
  expect_match(r3$text, "Turn 3", fixed = TRUE)
  # session uuid must be stable across turns
  expect_true(!is.null(client$get_session_id()))
})

test_that("error scenario surfaces as cli_abort", {
  client <- mock_claude_client("error")
  expect_error(client$prompt("boom"))
})

test_that("streaming on_text callback fires chunk-by-chunk", {
  chunks <- character()
  client <- mock_claude_client(
    "basic",
    on_text = function(chunk) chunks <<- c(chunks, chunk)
  )
  resp <- client$prompt("stream me")
  expect_true(length(chunks) >= 2L)  # basic emits two chunks
  expect_identical(paste(chunks, collapse = ""), resp$text)
})

test_that("HalChat wraps the Claude mock client", {
  chat <- mock_claude_chat("echo")
  reply <- chat$chat("round trip")
  expect_identical(reply, "round trip")
  turns <- chat$get_turns()
  expect_true(length(turns) >= 2L)  # user + assistant
})

test_that("backend factory routes to HalClientClaude under hal.backend='claude'", {
  withr::with_options(list(hal.backend = "claude"), {
    b <- hal:::.hal_backend()
    expect_identical(b, "claude")
    # Factory respects backend
    m <- hal:::.hal_default_model("claude")
    expect_identical(m, "sonnet")  # alias -> latest Sonnet
  })
})

test_that("tool_roundtrip scenario threads status back to matching tool_call", {
  client <- mock_claude_client("tool_roundtrip")
  resp <- client$prompt("run tools")
  expect_length(resp$tool_calls, 2L)

  ok <- Filter(function(tc) identical(tc$tool_call_id, "tool_rt_ok"), resp$tool_calls)
  err <- Filter(function(tc) identical(tc$tool_call_id, "tool_rt_err"), resp$tool_calls)
  expect_length(ok, 1L)
  expect_length(err, 1L)
  expect_identical(ok[[1]]$status, "completed")
  expect_identical(err[[1]]$status, "failed")
  expect_identical(ok[[1]]$result, "file contents here")
  expect_identical(err[[1]]$result, "file not found")
})

test_that("rate_limit_event is captured and surfaced via hal_quota()", {
  client <- mock_claude_client("rate_limit")
  resp <- client$prompt("ping")
  expect_identical(resp$text, "OK.")

  rl <- client$get_rate_limit()
  expect_false(is.null(rl))
  expect_identical(rl$rateLimitType, "five_hour")
  expect_identical(rl$status, "allowed")
  expect_identical(as.integer(rl$resetsAt), 1800000000L)
})

test_that("hal_quota() returns NULL on Copilot backend", {
  withr::with_options(list(hal.backend = "copilot"), {
    expect_message(q <- hal_quota(), "only available on the Claude backend")
    expect_null(q)
  })
})

test_that("print.hal_quota runs without error", {
  q <- structure(
    list(
      type = "five_hour",
      status = "allowed",
      resets_at = as.POSIXct(1800000000, origin = "1970-01-01"),
      overage_status = "allowed",
      overage_disabled_reason = NA_character_,
      backend = "claude"
    ),
    class = c("hal_quota", "list")
  )
  expect_invisible(print(q))
})

test_that("permission_args maps auto-allow to bypassPermissions", {
  client <- mock_claude_client("basic", permission_policy = "auto-allow")
  args <- client$.__enclos_env__$private$permission_args()
  expect_identical(args, c("--permission-mode", "bypassPermissions"))
})

test_that("permission_args maps auto-deny to default", {
  client <- mock_claude_client("basic", permission_policy = "auto-deny")
  args <- client$.__enclos_env__$private$permission_args()
  expect_identical(args, c("--permission-mode", "default"))
})

test_that("permission_args passes through raw mode strings", {
  client <- mock_claude_client("basic", permission_policy = "acceptEdits")
  args <- client$.__enclos_env__$private$permission_args()
  expect_identical(args, c("--permission-mode", "acceptEdits"))
})

test_that("permission_args wires function policies to permission_prompt tool", {
  fn <- function(params) "allow-once"
  client <- mock_claude_client("basic", permission_policy = fn)
  args <- client$.__enclos_env__$private$permission_args()
  expect_identical(args, c(
    "--permission-prompt-tool", "mcp__r-tools__permission_prompt",
    "--permission-mode", "default"
  ))
})

test_that("a mid-stream CLI failure leaves the session resumable", {
  # Regression: the client used to mark the session created only after a
  # *successful* turn, so a timeout or crash mid-stream left it re-sending
  # --session-id with a uuid the CLI had already provisioned -- rejected on
  # every subsequent call until hal_reset().
  client <- mock_claude_client("die_after_init")
  client$new_session()
  sid <- client$get_session_id()

  expect_error(suppressMessages(capture.output(client$prompt("this dies"))))

  expect_true(client$.__enclos_env__$private$session_created)
  expect_identical(client$get_session_id(), sid)
})

test_that("session is marked created once the CLI announces it", {
  client <- mock_claude_client("basic")
  client$new_session()
  expect_false(client$.__enclos_env__$private$session_created)
  client$prompt("hello")
  expect_true(client$.__enclos_env__$private$session_created)
})

test_that("time spent servicing eval_r is credited back to the prompt deadline", {
  # Regression: the deadline only reset on CLI output, but eval_r runs
  # synchronously in the same loop -- so a slow tool call burned the prompt
  # budget and aborted turns that were progressing normally.
  client <- mock_claude_client("quiet_gap")

  ipc_dir <- file.path(tempdir(), "hal_ipc_deadline_test")
  dir.create(ipc_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(ipc_dir, recursive = TRUE), add = TRUE)

  # Two queued tool calls, each taking ~1.5s of user R code.
  for (i in 1:2) {
    writeLines(
      as.character(jsonlite::toJSON(
        list(id = paste0("req", i), kind = "eval", code = "1 + 1"),
        auto_unbox = TRUE
      )),
      file.path(ipc_dir, paste0("request-", i, ".json"))
    )
  }
  client$set_ipc(ipc_dir, eval_fn = function(code) {
    Sys.sleep(1.5)
    list(result = "ok", error = NULL)
  })

  # The mock goes quiet for 4s, longer than this 3s timeout. The ~3s spent
  # in eval must be credited back or the turn aborts mid-flight.
  resp <- client$prompt("go", timeout = 3)
  expect_match(resp$text, "done.", fixed = TRUE)
})

test_that("process_ipc reports how many requests it handled", {
  # The prompt loop credits this count back to the response deadline, so it
  # has to be a number even when no IPC directory is wired up.
  client <- mock_claude_client("basic")
  expect_identical(client$.__enclos_env__$private$process_ipc(), 0L)
})

test_that("uuid_v4 does not consume or depend on user RNG state", {
  set.seed(42)
  u1 <- hal:::uuid_v4()
  saved_state <- .Random.seed
  set.seed(42)
  u2 <- hal:::uuid_v4()
  # Two calls with same seed must produce DIFFERENT uuids (seed-independent)
  expect_false(identical(u1, u2))
  # And hal:::uuid_v4() must not clobber the user's RNG stream
  expect_identical(.Random.seed, saved_state)
})
