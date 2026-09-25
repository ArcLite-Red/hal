# Tests for HalClient$register_tools() merge behavior (fix #2)
#
# HalClient$new() calls find_hal_cli() which aborts if no CLI is
# found. We pass cli_path pointing to a real executable (Rscript) so the
# constructor succeeds — we never actually start the subprocess.

test_that("register_tools() merges tools by name instead of replacing", {
  rscript <- as.character(Sys.which("Rscript"))
  skip_if(!nzchar(rscript) || !file.exists(rscript), "Rscript not found")
  client <- HalClient$new(cli_path = rscript)

  tool_a <- list(name = "tool_a", description = "first", fun = identity)
  tool_b <- list(name = "tool_b", description = "second", fun = identity)
  tool_c <- list(name = "tool_c", description = "third", fun = identity)

  # First batch
  client$register_tools(list(tool_a = tool_a, tool_b = tool_b))

  # Second batch — should add tool_c, not replace tool_a/tool_b
  client$register_tools(list(tool_c = tool_c))

  # Access private field via R6 enclosing environment
  env <- client$.__enclos_env__$private
  expect_length(env$registered_tools, 3)
  expect_true(all(c("tool_a", "tool_b", "tool_c") %in% names(env$registered_tools)))
})

test_that("hal_register_tools() routes a list to the session chat and tracks names", {
  got <- NULL
  fake_chat <- list(register_tools = function(tools) { got <<- tools; invisible() })

  sess <- .hal_get_session()
  old_chat <- sess$chat
  old_names <- sess$tool_names
  on.exit({ sess$chat <- old_chat; sess$tool_names <- old_names }, add = TRUE)
  sess$chat <- fake_chat          # non-null -> .hal_ensure_session won't initialize a backend
  sess$tool_names <- character()

  n <- hal_register_tools(list(
    list(name = "a", description = "x", fun = identity),
    list(name = "b", description = "y", fun = identity)
  ))

  expect_equal(n, 2L)
  expect_length(got, 2L)
  expect_true(all(c("a", "b") %in% sess$tool_names))
})

test_that(".hal_tool_name reads name from a plain list", {
  expect_equal(.hal_tool_name(list(name = "foo", fun = identity)), "foo")
  expect_equal(.hal_tool_name(list(id = "bar", fun = identity)), "bar")
})

test_that("register_tools() updates existing tools by name", {
  rscript <- as.character(Sys.which("Rscript"))
  skip_if(!nzchar(rscript) || !file.exists(rscript), "Rscript not found")
  client <- HalClient$new(cli_path = rscript)

  tool_v1 <- list(name = "my_tool", description = "version 1", fun = identity)
  tool_v2 <- list(name = "my_tool", description = "version 2", fun = identity)

  client$register_tools(list(my_tool = tool_v1))
  client$register_tools(list(my_tool = tool_v2))

  env <- client$.__enclos_env__$private
  expect_length(env$registered_tools, 1)
  expect_equal(env$registered_tools$my_tool$description, "version 2")
})
