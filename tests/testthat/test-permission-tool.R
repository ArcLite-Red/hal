# Tests for the permission_prompt MCP bridge.
#
# Covers:
#   - tool definition shape
#   - .hal_ipc_permission_fn behavior (params synthesis, return mapping,
#     error fallback, no-policy fallback)
#   - process_ipc kind dispatch on both backends

# =============================================================================
# Tool definition
# =============================================================================

test_that("permission_prompt tool def has the expected shape", {
  td <- hal:::.hal_permission_prompt_tool_def()
  expect_equal(td$name, "permission_prompt")
  expect_true(nzchar(td$description))
  expect_true(is.function(td$fun))
  expect_equal(td$parameters$type, "object")
  expect_true("tool_name" %in% names(td$parameters$properties))
  expect_true("input" %in% names(td$parameters$properties))
})

test_that("permission_prompt local fallback denies", {
  td <- hal:::.hal_permission_prompt_tool_def()
  out <- td$fun(tool_name = "Edit", input = list())
  parsed <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_equal(parsed$behavior, "deny")
})

# =============================================================================
# Parent-side IPC handler
# =============================================================================

test_that("permission handler returns deny when no policy is registered", {
  session <- hal:::.hal_get_session()
  saved <- session$permission_policy_fn
  withr::defer(session$permission_policy_fn <- saved)
  session$permission_policy_fn <- NULL

  out <- hal:::.hal_ipc_permission_fn(list(
    id = "p1", tool_name = "Edit", input = list(path = "x.R")
  ))
  parsed <- jsonlite::fromJSON(out$result, simplifyVector = FALSE)
  expect_equal(parsed$behavior, "deny")
  expect_null(out$error)
})

test_that("permission handler routes allow decisions to behavior=allow", {
  session <- hal:::.hal_get_session()
  saved <- session$permission_policy_fn
  withr::defer(session$permission_policy_fn <- saved)

  captured <- NULL
  session$permission_policy_fn <- function(params) {
    captured <<- params
    "allow-once"
  }

  out <- hal:::.hal_ipc_permission_fn(list(
    id = "p2", tool_name = "Edit", input = list(path = "x.R", contents = "1+1")
  ))
  parsed <- jsonlite::fromJSON(out$result, simplifyVector = FALSE)
  expect_equal(parsed$behavior, "allow")
  # Claude's contract requires updatedInput on allow — must echo input back.
  expect_equal(parsed$updatedInput$path, "x.R")
  expect_equal(parsed$updatedInput$contents, "1+1")

  # Honest Claude-shaped params (no synthetic Copilot envelope)
  expect_equal(captured$backend, "claude")
  expect_equal(captured$tool_name, "Edit")
  expect_equal(captured$input$path, "x.R")
  expect_equal(captured$input$contents, "1+1")
})

test_that("permission handler routes reject decisions to behavior=deny", {
  session <- hal:::.hal_get_session()
  saved <- session$permission_policy_fn
  withr::defer(session$permission_policy_fn <- saved)
  session$permission_policy_fn <- function(params) "reject-once"

  out <- hal:::.hal_ipc_permission_fn(list(
    id = "p3", tool_name = "Bash", input = list(command = "rm -rf /")
  ))
  parsed <- jsonlite::fromJSON(out$result, simplifyVector = FALSE)
  expect_equal(parsed$behavior, "deny")
})

test_that("permission handler denies when policy errors", {
  session <- hal:::.hal_get_session()
  saved <- session$permission_policy_fn
  withr::defer(session$permission_policy_fn <- saved)
  session$permission_policy_fn <- function(params) stop("policy boom")

  expect_warning(
    out <- hal:::.hal_ipc_permission_fn(list(
      id = "p4", tool_name = "Edit", input = list()
    )),
    "permission_policy function errored"
  )
  parsed <- jsonlite::fromJSON(out$result, simplifyVector = FALSE)
  expect_equal(parsed$behavior, "deny")
})

test_that("permission handler denies on invalid policy return value", {
  session <- hal:::.hal_get_session()
  saved <- session$permission_policy_fn
  withr::defer(session$permission_policy_fn <- saved)
  session$permission_policy_fn <- function(params) list("oops")

  out <- hal:::.hal_ipc_permission_fn(list(
    id = "p5", tool_name = "Edit", input = list()
  ))
  parsed <- jsonlite::fromJSON(out$result, simplifyVector = FALSE)
  expect_equal(parsed$behavior, "deny")
})

# =============================================================================
# Client process_ipc kind dispatch (Copilot)
# =============================================================================

test_that("HalClient process_ipc dispatches kind=permission to permission_fn", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )

  ipc_dir <- tempfile("test_ipc_perm_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  perm_called <- FALSE
  perm_req <- NULL

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(
    ipc_dir,
    eval_fn = function(code) list(result = "eval-ran", error = NULL),
    permission_fn = function(req) {
      perm_called <<- TRUE
      perm_req <<- req
      list(result = '{"behavior":"allow"}', error = NULL)
    }
  )

  req <- jsonlite::toJSON(
    list(id = "perm-001", kind = "permission",
         tool_name = "Edit", input = list(path = "x.R")),
    auto_unbox = TRUE
  )
  writeLines(as.character(req),
             file.path(ipc_dir, "request-perm-001.json"))

  client$.__enclos_env__$private$process_ipc()

  expect_true(perm_called)
  expect_equal(perm_req$tool_name, "Edit")
  expect_equal(perm_req$input$path, "x.R")

  resp_file <- file.path(ipc_dir, "response-perm-001.json")
  expect_true(file.exists(resp_file))

  resp <- jsonlite::fromJSON(readLines(resp_file, warn = FALSE),
                              simplifyVector = FALSE)
  expect_equal(resp$result, '{"behavior":"allow"}')
})

test_that("HalClient process_ipc still dispatches kind=eval", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )

  ipc_dir <- tempfile("test_ipc_eval_kind_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(
    ipc_dir,
    eval_fn = function(code) list(result = paste0("got:", code), error = NULL),
    permission_fn = function(req) stop("should not reach")
  )

  req <- jsonlite::toJSON(
    list(id = "eval-002", kind = "eval", code = "2 + 2"),
    auto_unbox = TRUE
  )
  writeLines(as.character(req),
             file.path(ipc_dir, "request-eval-002.json"))

  client$.__enclos_env__$private$process_ipc()

  resp <- jsonlite::fromJSON(
    readLines(file.path(ipc_dir, "response-eval-002.json"), warn = FALSE),
    simplifyVector = FALSE
  )
  expect_equal(resp$result, "got:2 + 2")
})

test_that("HalClient process_ipc reports error when no handler for kind", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )

  ipc_dir <- tempfile("test_ipc_unknown_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(ipc_dir,
    eval_fn = function(code) list(result = "x", error = NULL))
  # permission_fn intentionally NULL

  req <- jsonlite::toJSON(
    list(id = "perm-noh", kind = "permission",
         tool_name = "Edit", input = list()),
    auto_unbox = TRUE
  )
  writeLines(as.character(req),
             file.path(ipc_dir, "request-perm-noh.json"))

  client$.__enclos_env__$private$process_ipc()

  resp <- jsonlite::fromJSON(
    readLines(file.path(ipc_dir, "response-perm-noh.json"), warn = FALSE),
    simplifyVector = FALSE
  )
  expect_match(resp$error, "No IPC handler for kind: permission")
})

# =============================================================================
# Claude client process_ipc kind dispatch
# =============================================================================

test_that("HalClientClaude process_ipc dispatches kind=permission", {
  local_mocked_bindings(
    find_claude_cli = function(...) list(command = "fake", prefix_args = character())
  )

  ipc_dir <- tempfile("test_ipc_claude_perm_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  perm_called <- FALSE
  client <- hal:::HalClientClaude$new(quiet = TRUE)
  client$set_ipc(
    ipc_dir,
    eval_fn = function(code) list(result = "x", error = NULL),
    permission_fn = function(req) {
      perm_called <<- TRUE
      list(result = '{"behavior":"deny"}', error = NULL)
    }
  )

  req <- jsonlite::toJSON(
    list(id = "claude-perm", kind = "permission",
         tool_name = "Bash", input = list(command = "ls")),
    auto_unbox = TRUE
  )
  writeLines(as.character(req),
             file.path(ipc_dir, "request-claude-perm.json"))

  client$.__enclos_env__$private$process_ipc()

  expect_true(perm_called)
  resp <- jsonlite::fromJSON(
    readLines(file.path(ipc_dir, "response-claude-perm.json"), warn = FALSE),
    simplifyVector = FALSE
  )
  expect_equal(resp$result, '{"behavior":"deny"}')
})
