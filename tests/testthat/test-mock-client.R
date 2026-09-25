# Tests for HalClient and HalChat via mock Copilot CLI
#
# These tests exercise the full NDJSON transport layer offline.
# The mock CLI (inst/mock-cli/mock_copilot.R) handles different scenarios.

# =============================================================================
# Tier 1: Core transport (HalClient)
# =============================================================================

test_that("handshake completes with mock CLI", {
  client <- mock_client("basic")
  withr::defer(client$stop())

  result <- client$handshake()

  expect_true(client$is_alive())
  expect_equal(result$agentInfo$name, "MockCopilot")
  expect_equal(result$agentInfo$version, "0.0.1")
  expect_equal(result$protocolVersion, 1)
})

test_that("new_session returns session ID and models", {
  client <- mock_client("basic")
  withr::defer(client$stop())

  result <- client$new_session()

  expect_equal(client$get_session_id(), "mock-session-001")
  expect_equal(result$models$currentModelId, "mock-model")
  expect_true(length(result$models$availableModels) >= 1)
})

test_that("single prompt returns hal_response", {
  client <- mock_client("basic")
  withr::defer(client$stop())

  resp <- client$prompt("Hello")

  expect_s3_class(resp, "hal_response")
  expect_equal(resp$text, "Hello from MockCopilot.")
  expect_equal(resp$stop_reason, "end_turn")
  expect_true(length(resp$events) > 0)
})

test_that("echo scenario returns prompt text", {
  client <- mock_client("echo")
  withr::defer(client$stop())

  resp <- client$prompt("Test echo 123")

  expect_s3_class(resp, "hal_response")
  expect_equal(resp$text, "Test echo 123")
})

test_that("multi-turn prompts work sequentially", {
  client <- mock_client("multi_turn")
  withr::defer(client$stop())

  resp1 <- client$prompt("First")
  resp2 <- client$prompt("Second")

  expect_equal(resp1$text, "Turn 1 response.")
  expect_equal(resp2$text, "Turn 2 response.")
})

test_that("stop kills process and cleans up", {
  client <- mock_client("basic")
  client$start()

  expect_true(client$is_alive())
  client$stop()
  expect_false(client$is_alive())
})

# =============================================================================
# Tier 2: HalChat integration
# =============================================================================

test_that("HalChat$chat returns text", {
  chat <- mock_chat("basic")
  withr::defer(chat$get_client()$stop())

  result <- chat$chat("Hello")

  expect_type(result, "character")
  expect_equal(result, "Hello from MockCopilot.")
})

test_that("HalChat tracks turns", {
  chat <- mock_chat("basic")
  withr::defer(chat$get_client()$stop())

  chat$chat("First question")

  turns <- chat$get_turns()
  expect_length(turns, 2)  # user + assistant
  expect_equal(turns[[1]]$role, "user")
  expect_equal(turns[[2]]$role, "assistant")
})

test_that("HalChat system prompt is prepended on first turn", {
  client <- mock_client("echo")
  chat <- HalChat$new(client = client, system_prompt = "You are helpful.",
                      echo = "none", quiet = TRUE)
  withr::defer(client$stop())

  result <- chat$chat("Hello")

  # Echo scenario returns whatever was sent — should include system prompt
  expect_match(result, "You are helpful")
  expect_match(result, "Hello")
})

test_that("HalChat multi-turn with turn counting", {
  chat <- mock_chat("multi_turn")
  withr::defer(chat$get_client()$stop())

  chat$chat("A")
  chat$chat("B")
  chat$chat("C")

  turns <- chat$get_turns()
  expect_length(turns, 6)  # 3 user + 3 assistant

  last <- chat$last_turn()
  expect_equal(last$role, "assistant")
})

test_that("HalChat$last_response returns hal_response", {
  chat <- mock_chat("basic")
  withr::defer(chat$get_client()$stop())

  chat$chat("Hello")

  resp <- chat$last_response()
  expect_s3_class(resp, "hal_response")
  expect_equal(resp$stop_reason, "end_turn")
})

# =============================================================================
# Tier 3: Event handling
# =============================================================================

test_that("tool_call events are captured on response", {
  client <- mock_client("tool_call")
  withr::defer(client$stop())

  resp <- client$prompt("Read the README")

  expect_equal(resp$text, "Let me check. Done.")
  expect_length(resp$tool_calls, 1)

  tc <- resp$tool_calls[[1]]
  expect_s3_class(tc, "hal_tool_call")
  expect_equal(tc$tool_call_id, "tc-001")
  expect_equal(tc$title, "view README.md")
  expect_equal(tc$kind, "read")
  expect_equal(tc$status, "completed")
})

test_that("thought chunks are captured on response", {
  client <- mock_client("thoughts")
  withr::defer(client$stop())

  resp <- client$prompt("Think about this")

  expect_equal(resp$text, "Here is my answer.")
  expect_length(resp$thoughts, 2)
  expect_equal(resp$thoughts[1], "Let me think...")
  expect_equal(resp$thoughts[2], " Okay, I know.")
})

test_that("permission request auto-allow works", {
  client <- mock_client("permission", permission_policy = "auto-allow")
  withr::defer(client$stop())

  resp <- client$prompt("Edit a file")

  expect_s3_class(resp, "hal_response")
  expect_equal(resp$text, "Permission handled.")
})

test_that("permission request auto-deny works", {
  client <- mock_client("permission", permission_policy = "auto-deny")
  withr::defer(client$stop())

  resp <- client$prompt("Edit a file")

  expect_s3_class(resp, "hal_response")
  expect_equal(resp$text, "Permission handled.")
})

test_that("function permission_policy is invoked with the request params", {
  captured <- NULL
  policy <- function(params) {
    captured <<- params
    "allow-once"
  }
  client <- mock_client("permission", permission_policy = policy)
  withr::defer(client$stop())

  resp <- client$prompt("Edit a file")

  expect_s3_class(resp, "hal_response")
  expect_equal(resp$text, "Permission handled.")
  expect_false(is.null(captured))
  expect_equal(captured$toolCall$title, "edit test.R")
  expect_equal(captured$toolCall$kind, "write")
  expect_true(length(captured$options) >= 1L)
  option_ids <- vapply(captured$options, `[[`, character(1), "optionId")
  expect_true("allow-once" %in% option_ids)
  expect_true("reject-once" %in% option_ids)
})

# =============================================================================
# Tier 4: Session management and edge cases
# =============================================================================

test_that("switch_model sends request and succeeds", {
  client <- mock_client("basic")
  withr::defer(client$stop())

  # Establish session first
  client$new_session()

  # Should not error
  expect_no_error(client$switch_model("gpt-4.1"))
})

test_that("set_mode sends request and succeeds", {
  client <- mock_client("basic")
  withr::defer(client$stop())

  client$new_session()
  expect_no_error(client$set_mode("plan"))
})

test_that("error scenario raises R error", {
  client <- mock_client("error")
  withr::defer(client$stop())

  expect_error(client$prompt("trigger error"), "Mock error")
})

test_that("on_text callback fires for each chunk", {
  chunks <- character()
  client <- mock_client("basic", on_text = function(chunk) {
    chunks <<- c(chunks, chunk)
  })
  withr::defer(client$stop())

  client$prompt("Hello")

  expect_length(chunks, 2)
  expect_equal(chunks[1], "Hello from ")
  expect_equal(chunks[2], "MockCopilot.")
})

test_that("on_tool_call callback fires for tool events", {
  calls <- list()
  client <- mock_client("tool_call", on_tool_call = function(tc) {
    calls <<- c(calls, list(tc))
  })
  withr::defer(client$stop())

  client$prompt("Read it")

  # Should fire twice: once for tool_call (pending), once for tool_call_update (completed)
  expect_length(calls, 2)
  expect_equal(calls[[1]]$status, "pending")
  expect_equal(calls[[2]]$status, "completed")
})

test_that("on_thought callback fires for thought chunks", {
  thoughts <- character()
  client <- mock_client("thoughts", on_thought = function(chunk) {
    thoughts <<- c(thoughts, chunk)
  })
  withr::defer(client$stop())

  client$prompt("Think")

  expect_length(thoughts, 2)
  expect_equal(thoughts[1], "Let me think...")
})

test_that("HalChat switch_model delegates to client", {
  chat <- mock_chat("basic")
  withr::defer(chat$get_client()$stop())

  chat$chat("Hello")
  expect_no_error(chat$switch_model("gpt-4.1"))
})
