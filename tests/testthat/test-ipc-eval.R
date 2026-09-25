# Tests for IPC eval function and file operations

# =============================================================================
# IPC eval function
# =============================================================================

test_that("ipc_eval_fn executes simple code", {
  result <- .hal_ipc_eval_fn("1 + 1")

  expect_null(result$error)
  expect_match(result$result, "2")
})

test_that("ipc_eval_fn returns error for bad code", {
  result <- .hal_ipc_eval_fn("nonexistent_var_xyz")

  expect_false(is.null(result$error))
  expect_match(result$error, "Error")
})

test_that("ipc_eval_fn enforces denylist", {
  result <- .hal_ipc_eval_fn("system('echo hi')")

  # Should be blocked (not an error, but a blocked message)
  expect_null(result$error)
  expect_match(result$result, "blocked|denylist", ignore.case = TRUE)
})

test_that("ipc_eval_fn can see caller env objects", {
  session <- .hal_get_session()
  old_env <- session$eval_caller_env
  test_env <- new.env(parent = globalenv())
  test_env$visible_var <- "I am here"
  session$eval_caller_env <- test_env

  result <- .hal_ipc_eval_fn("visible_var")

  expect_null(result$error)
  expect_match(result$result, "I am here")

  session$eval_caller_env <- old_env
})

test_that("ipc_eval_fn can modify caller env objects", {
  session <- .hal_get_session()
  old_env <- session$eval_caller_env
  test_env <- new.env(parent = globalenv())
  test_env$x <- 10
  session$eval_caller_env <- test_env

  result <- .hal_ipc_eval_fn("x <- x * 2")

  expect_null(result$error)
  expect_equal(test_env$x, 20)

  session$eval_caller_env <- old_env
})

test_that("ipc_eval_fn can create new objects in caller env", {
  session <- .hal_get_session()
  old_env <- session$eval_caller_env
  test_env <- new.env(parent = globalenv())
  session$eval_caller_env <- test_env

  result <- .hal_ipc_eval_fn("new_item <- 'hello'")

  expect_null(result$error)
  expect_equal(test_env$new_item, "hello")

  session$eval_caller_env <- old_env
})

test_that("ipc_eval_fn tracks new assignments", {
  session <- .hal_get_session()
  old_env <- session$eval_caller_env
  old_assigned <- session$eval_r_assigned
  test_env <- new.env(parent = globalenv())
  session$eval_caller_env <- test_env
  session$eval_r_assigned <- character()

  .hal_ipc_eval_fn("new_var <- 42")

  expect_true("new_var" %in% session$eval_r_assigned)

  session$eval_caller_env <- old_env
  session$eval_r_assigned <- old_assigned
})

test_that("ipc_eval_fn reports new variables in output", {
  session <- .hal_get_session()
  old_env <- session$eval_caller_env
  test_env <- new.env(parent = globalenv())
  session$eval_caller_env <- test_env

  result <- .hal_ipc_eval_fn("brand_new <- 123")

  expect_null(result$error)
  expect_match(result$result, "brand_new")

  session$eval_caller_env <- old_env
})

# =============================================================================
# IPC file operations
# =============================================================================

test_that("client process_ipc handles request files", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )

  # Create a client with IPC enabled
  ipc_dir <- tempfile("test_ipc_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  eval_called <- FALSE
  eval_code <- NULL

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(ipc_dir, function(code) {
    eval_called <<- TRUE
    eval_code <<- code
    list(result = "test_output", error = NULL)
  })

  # Write a fake request
  req <- jsonlite::toJSON(
    list(id = "test-001", code = "1 + 1"),
    auto_unbox = TRUE
  )
  writeLines(as.character(req),
             file.path(ipc_dir, "request-test-001.json"))

  # Process it (call private method via enclosure)
  client$.__enclos_env__$private$process_ipc()

  expect_true(eval_called)
  expect_equal(eval_code, "1 + 1")

  # Check response was written
  resp_file <- file.path(ipc_dir, "response-test-001.json")
  expect_true(file.exists(resp_file))

  resp <- jsonlite::fromJSON(readLines(resp_file, warn = FALSE),
                              simplifyVector = FALSE)
  expect_equal(resp$id, "test-001")
  expect_equal(resp$result, "test_output")
  expect_null(resp$error)
})

test_that("client process_ipc handles eval errors", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )

  ipc_dir <- tempfile("test_ipc_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(ipc_dir, function(code) {
    list(result = NULL, error = "something broke")
  })

  req <- jsonlite::toJSON(
    list(id = "test-err", code = "bad"),
    auto_unbox = TRUE
  )
  writeLines(as.character(req),
             file.path(ipc_dir, "request-test-err.json"))

  client$.__enclos_env__$private$process_ipc()

  resp_file <- file.path(ipc_dir, "response-test-err.json")
  expect_true(file.exists(resp_file))

  resp <- jsonlite::fromJSON(readLines(resp_file, warn = FALSE),
                              simplifyVector = FALSE)
  expect_equal(resp$error, "something broke")
  expect_null(resp$result)
})

test_that("client process_ipc skips malformed requests", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )

  ipc_dir <- tempfile("test_ipc_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(ipc_dir, function(code) {
    list(result = "ok", error = NULL)
  })

  # Write a malformed request (no id or code)
  writeLines("not json", file.path(ipc_dir, "request-bad.json"))

  # Should not error
  expect_no_error(client$.__enclos_env__$private$process_ipc())

  # Malformed file should be cleaned up
  expect_false(file.exists(file.path(ipc_dir, "request-bad.json")))
})

test_that("client process_ipc is no-op when IPC not configured", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )

  client <- HalClient$new(quiet = TRUE)
  # No set_ipc call -- ipc_dir is NULL
  expect_no_error(client$.__enclos_env__$private$process_ipc())
})

test_that("client stop cleans up IPC directory", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )

  ipc_dir <- tempfile("test_ipc_")
  dir.create(ipc_dir)

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(ipc_dir, function(code) list(result = "", error = NULL))

  expect_true(dir.exists(ipc_dir))
  client$stop()
  expect_false(dir.exists(ipc_dir))
})

# ------------------------------------------------------------------------------
# Plot vision -- image field rides the IPC response
# ------------------------------------------------------------------------------

test_that("process_ipc passes image field through the response JSON", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )
  ipc_dir <- tempfile("test_ipc_img_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(ipc_dir, function(code) {
    list(result = "plotted", error = NULL,
         image = list(mimeType = "image/png", data = "QUJDRA=="))
  })

  req <- jsonlite::toJSON(list(id = "img-001", code = "plot(1)"),
                          auto_unbox = TRUE)
  writeLines(as.character(req), file.path(ipc_dir, "request-img-001.json"))
  client$.__enclos_env__$private$process_ipc()

  resp <- jsonlite::fromJSON(
    readLines(file.path(ipc_dir, "response-img-001.json"), warn = FALSE),
    simplifyVector = FALSE
  )
  expect_identical(resp$result, "plotted")
  expect_identical(resp$image$data, "QUJDRA==")
  expect_identical(resp$image$mimeType, "image/png")
})

test_that("process_ipc omits image field when handler returns none", {
  local_mocked_bindings(
    find_hal_cli = function(...) list(command = "fake", prefix_args = character())
  )
  ipc_dir <- tempfile("test_ipc_noimg_")
  dir.create(ipc_dir)
  withr::defer(unlink(ipc_dir, recursive = TRUE))

  client <- HalClient$new(quiet = TRUE)
  client$set_ipc(ipc_dir, function(code) list(result = "ok", error = NULL))

  req <- jsonlite::toJSON(list(id = "no-001", code = "1"), auto_unbox = TRUE)
  writeLines(as.character(req), file.path(ipc_dir, "request-no-001.json"))
  client$.__enclos_env__$private$process_ipc()

  resp <- jsonlite::fromJSON(
    readLines(file.path(ipc_dir, "response-no-001.json"), warn = FALSE),
    simplifyVector = FALSE
  )
  expect_null(resp$image)
})

# ------------------------------------------------------------------------------
# Generated MCP script -- %||% definition + backend-gated image block
# ------------------------------------------------------------------------------

test_that("generated MCP script defines %||% and parses", {
  mcp <- build_mcp_script(
    list(eval_r = list(name = "eval_r", description = "d", fun = identity)),
    ipc_dir = tempdir(), backend = "claude"
  )
  withr::defer(unlink(c(mcp$script_path, mcp$tools_path)))
  script <- readLines(mcp$script_path)
  expect_true(any(grepl("%||%", script, fixed = TRUE) &
                    grepl("<- function", script, fixed = TRUE)))
  expect_no_error(parse(mcp$script_path))
})

test_that("MCP image block is emitted for claude, gated off for copilot", {
  tools <- list(eval_r = list(name = "eval_r", description = "d",
                              fun = identity))
  mcp_claude <- build_mcp_script(tools, ipc_dir = tempdir(),
                                 backend = "claude")
  mcp_copilot <- build_mcp_script(tools, ipc_dir = tempdir(),
                                  backend = "copilot")
  withr::defer(unlink(c(mcp_claude$script_path, mcp_claude$tools_path,
                        mcp_copilot$script_path, mcp_copilot$tools_path)))

  claude_txt <- paste(readLines(mcp_claude$script_path), collapse = "\n")
  copilot_txt <- paste(readLines(mcp_copilot$script_path), collapse = "\n")

  # Both scripts contain the gate code; the backend constant decides.
  expect_match(claude_txt, 'backend <- "claude"')
  expect_match(copilot_txt, 'backend <- "copilot"')
  expect_match(claude_txt, 'type = "image"')
})
