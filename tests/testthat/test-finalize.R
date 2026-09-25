# Tests for HalClient finalize method (fix #8)
#
# When a HalClient is garbage-collected without $stop(), temp MCP files
# should still be cleaned up via the R6 finalize method.

test_that("finalize cleans up temp MCP files on garbage collection", {
  # Create temp files to simulate MCP artifacts
  config_file <- tempfile("test_mcp_config_", fileext = ".json")
  script_file <- tempfile("test_mcp_script_", fileext = ".R")
  tools_file <- tempfile("test_mcp_tools_", fileext = ".rds")
  writeLines("{}", config_file)
  writeLines("# script", script_file)
  saveRDS(list(), tools_file)

  expect_true(file.exists(config_file))
  expect_true(file.exists(script_file))
  expect_true(file.exists(tools_file))

  # Build a minimal R6 object with finalize and the private fields set.
  # We can't use HalClient$new() (it needs a CLI), so we construct
  # a lightweight R6 with the same finalize logic to test the mechanism.
  TestClient <- R6::R6Class(
    "TestClient",
    private = list(
      mcp_config_path = NULL,
      mcp_script_path = NULL,
      mcp_tools_path = NULL,
      finalize = function() {
        for (f in c(private$mcp_tools_path, private$mcp_script_path,
                     private$mcp_config_path)) {
          if (!is.null(f) && file.exists(f)) try(unlink(f), silent = TRUE)
        }
      }
    )
  )

  obj <- TestClient$new()
  obj$.__enclos_env__$private$mcp_config_path <- config_file
  obj$.__enclos_env__$private$mcp_script_path <- script_file
  obj$.__enclos_env__$private$mcp_tools_path <- tools_file

  # Remove reference and force GC
  rm(obj)
  gc()

  expect_false(file.exists(config_file))
  expect_false(file.exists(script_file))
  expect_false(file.exists(tools_file))
})

test_that("finalize handles missing files gracefully", {
  # Finalize should not error if files were already deleted
  TestClient <- R6::R6Class(
    "TestClient",
    private = list(
      mcp_config_path = NULL,
      mcp_script_path = NULL,
      mcp_tools_path = NULL,
      finalize = function() {
        for (f in c(private$mcp_tools_path, private$mcp_script_path,
                     private$mcp_config_path)) {
          if (!is.null(f) && file.exists(f)) try(unlink(f), silent = TRUE)
        }
      }
    )
  )

  obj <- TestClient$new()
  obj$.__enclos_env__$private$mcp_config_path <- "/nonexistent/path.json"
  obj$.__enclos_env__$private$mcp_script_path <- NULL

  # Should not error
  expect_no_error({
    rm(obj)
    gc()
  })
})
