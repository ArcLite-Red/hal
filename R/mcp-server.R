# Minimal MCP stdio server for exposing R tools to the Copilot CLI.
#
# The server is written as a self-contained R script (no hal dependency)
# so it can run in a fresh Rscript subprocess. build_mcp_config() generates
# a JSON config file + server script; the CLI loads it via
# --additional-mcp-config at startup.

#' Build a self-contained MCP server script and serialized tools
#'
#' Writes two temp files: an RDS with tool definitions (including functions)
#' and an R script that implements the MCP protocol. The script only depends
#' on jsonlite.
#'
#' @param tools Named list of tool definitions.
#' @return A list with `script_path`, `tools_path`, and `rscript` (full path).
#' @noRd
build_mcp_script <- function(tools, ipc_dir = NULL, backend = "copilot") {
  tools_path <- tempfile("hal_tools_", fileext = ".rds")
  script_path <- tempfile("hal_mcp_", fileext = ".R")

  # Normalize paths for cross-platform use in the script
  tools_path_safe <- gsub("\\\\", "/", tools_path)
  ipc_dir_safe <- if (!is.null(ipc_dir)) gsub("\\\\", "/", ipc_dir) else ""

  saveRDS(tools, tools_path)

  # Capture current library paths so the subprocess sees the same libs.

  # This fixes cases where R_LIBS_USER points to a broken/incomplete install
  # (e.g. missing DLLs) that shadows the working system library.
  lib_paths <- .libPaths()
  lib_paths_safe <- gsub("\\\\", "/", lib_paths)
  lib_paths_literal <- paste0(
    ".libPaths(c(",
    paste0('"', lib_paths_safe, '"', collapse = ", "),
    "))"
  )

  # Write self-contained MCP server script
  writeLines(con = script_path, c(
    '# Auto-generated MCP server - do not edit',
    lib_paths_literal,
    'library(jsonlite)',
    # base R only gained %||% in 4.4.0; hal supports R >= 4.1 and this
    # subprocess loads only jsonlite, so define it unconditionally.
    '`%||%` <- function(a, b) if (is.null(a)) b else a',
    paste0('tools <- readRDS("', tools_path_safe, '")'),
    paste0('ipc_dir <- "', ipc_dir_safe, '"'),
    paste0('backend <- "', backend, '"'),
    '',
    '# IPC: proxy eval_r calls to the parent R session via file exchange.',
    '# Atomic writes (tmp + rename) and read-retries protect against EDR/AV',
    '# file locks on Windows tempdir, which surface as "Permission denied".',
    'ipc_atomic_write <- function(path, text) {',
    '  tmp <- paste0(path, ".tmp-", Sys.getpid())',
    '  ok_write <- tryCatch({ writeLines(text, tmp); TRUE },',
    '                       error = function(e) FALSE)',
    '  if (!ok_write) return(FALSE)',
    '  for (i in seq_len(5)) {',
    '    ok <- suppressWarnings(file.rename(tmp, path))',
    '    if (isTRUE(ok)) return(TRUE)',
    '    Sys.sleep(0.05)',
    '  }',
    '  unlink(tmp)',
    '  FALSE',
    '}',
    'ipc_safe_read <- function(path) {',
    '  for (i in seq_len(5)) {',
    '    out <- tryCatch(',
    '      fromJSON(readLines(path, warn = FALSE), simplifyVector = FALSE),',
    '      error = function(e) e, warning = function(w) w',
    '    )',
    '    if (!inherits(out, c("error", "warning"))) return(out)',
    '    Sys.sleep(0.05)',
    '  }',
    '  NULL',
    '}',
    'ipc_request <- function(payload, prefix) {',
    '  if (!nzchar(ipc_dir)) return(NULL)',
    '  req_id <- paste0(prefix, "-", format(Sys.time(), "%H%M%OS3"), "-",',
    '                    sample.int(9999, 1))',
    '  payload$id <- req_id',
    '  req <- toJSON(payload, auto_unbox = TRUE, null = "null")',
    '  req_file <- file.path(ipc_dir, paste0("request-", req_id, ".json"))',
    '  if (!ipc_atomic_write(req_file, req)) {',
    '    return(list(result = NULL, error = "IPC request write failed (tempdir locked?)"))',
    '  }',
    '  resp_file <- file.path(ipc_dir, paste0("response-", req_id, ".json"))',
    '  deadline <- Sys.time() + 60',
    '  while (Sys.time() < deadline) {',
    '    if (file.exists(resp_file)) {',
    '      resp <- ipc_safe_read(resp_file)',
    '      unlink(resp_file)',
    '      if (is.null(resp)) return(list(result = NULL, error = "Failed to parse IPC response"))',
    '      return(resp)',
    '    }',
    '    Sys.sleep(0.1)',
    '  }',
    '  list(result = NULL, error = "IPC timeout: parent R session did not respond within 60s")',
    '}',
    'ipc_eval <- function(code) {',
    '  ipc_request(list(kind = "eval", code = code), "eval")',
    '}',
    'ipc_permission <- function(tool_name, input) {',
    '  ipc_request(list(kind = "permission", tool_name = tool_name,',
    '                   input = input), "perm")',
    '}',
    '',
    'send <- function(obj) {',
    '  msg <- toJSON(obj, auto_unbox = TRUE, null = "null")',
    '  cat(msg, "\\n", sep = "", file = stdout())',
    '  flush(stdout())',
    '}',
    '',
    'send_result <- function(id, result) {',
    '  send(list(jsonrpc = "2.0", id = id, result = result))',
    '}',
    '',
    'send_error <- function(id, code, message) {',
    '  send(list(jsonrpc = "2.0", id = id, error = list(code = code, message = message)))',
    '}',
    '',
    'handle <- function(msg) {',
    '  method <- msg$method',
    '  id <- msg$id',
    '',
    '  if (identical(method, "initialize")) {',
    '    send_result(id, list(',
    '      protocolVersion = "2025-03-26",',
    '      capabilities = list(tools = structure(list(), names = character())),',
    '      serverInfo = list(name = "hal", version = "0.1.0")',
    '    ))',
    '    return()',
    '  }',
    '',
    '  if (identical(method, "notifications/initialized")) return()',
    '',
    '  if (identical(method, "tools/list")) {',
    '    tool_defs <- lapply(tools, function(t) {',
    '      params <- t$schema[["function"]]$parameters',
    '      if (is.null(params)) params <- t$parameters',
    '      if (is.null(params)) params <- list(type = "object", properties = structure(list(), names = character()))',
    '      props <- params$properties',
    '      if (is.null(props) || (is.list(props) && length(props) == 0 && is.null(names(props)))) {',
    '        params$properties <- structure(list(), names = character())',
    '      }',
    '      list(name = t$name, description = t$description %||% "", inputSchema = params)',
    '    })',
    '    send_result(id, list(tools = unname(tool_defs)))',
    '    return()',
    '  }',
    '',
    '  if (identical(method, "tools/call")) {',
    '    tool_name <- msg$params$name',
    '    arguments <- msg$params$arguments',
    '    if (is.null(arguments)) arguments <- list()',
    '    tool <- tools[[tool_name]]',
    '    if (is.null(tool)) {',
    '      send_error(id, -32602, paste0("Unknown tool: ", tool_name))',
    '      return()',
    '    }',
    '    # Route eval_r through IPC to the parent R session',
    '    if (identical(tool_name, "eval_r") && nzchar(ipc_dir)) {',
    '      resp <- ipc_eval(arguments$code %||% "")',
    '      if (!is.null(resp$error)) {',
    '        send_result(id, list(content = list(list(type = "text", text = resp$error)), isError = TRUE))',
    '      } else {',
    '        content <- list(list(type = "text", text = resp$result %||% "(no output)"))',
    '        # Plot vision: attach captured plot as an MCP image block.',
    '        # Claude Code forwards image content to the model; the Copilot',
    '        # CLI is unverified, so it stays text-only (belt-and-braces --',
    '        # capture is already disabled at source for copilot).',
    '        if (identical(backend, "claude") && !is.null(resp$image$data)) {',
    '          content <- c(content, list(list(type = "image",',
    '            data = resp$image$data,',
    '            mimeType = resp$image$mimeType %||% "image/png")))',
    '        }',
    '        send_result(id, list(content = content, isError = FALSE))',
    '      }',
    '      return()',
    '    }',
    '    # Route permission_prompt through IPC; the parent invokes the user',
    '    # policy and returns a JSON-encoded {behavior, ...} payload.',
    '    if (identical(tool_name, "permission_prompt") && nzchar(ipc_dir)) {',
    '      resp <- ipc_permission(arguments$tool_name %||% "",',
    '                             arguments$input %||% list())',
    '      if (!is.null(resp$error)) {',
    '        send_result(id, list(content = list(list(type = "text", text = resp$error)), isError = TRUE))',
    '      } else {',
    '        send_result(id, list(content = list(list(type = "text", text = resp$result %||% "{\\"behavior\\":\\"deny\\"}")), isError = FALSE))',
    '      }',
    '      return()',
    '    }',
    '    result <- tryCatch({',
    '      output <- do.call(tool$fun, as.list(arguments))',
    '      txt <- if (is.character(output) && length(output) == 1L && !is.na(output)) output else as.character(jsonlite::toJSON(output, dataframe = "rows", auto_unbox = TRUE, na = "null", null = "null", POSIXt = "ISO8601"))',
    '      list(content = list(list(type = "text", text = txt)), isError = FALSE)',
    '    }, error = function(e) {',
    '      list(content = list(list(type = "text", text = conditionMessage(e))), isError = TRUE)',
    '    })',
    '    send_result(id, result)',
    '    return()',
    '  }',
    '',
    '  if (!is.null(id)) send_error(id, -32601, paste0("Method not found: ", method))',
    '}',
    '',
    'con <- file("stdin", open = "r")',
    'while (TRUE) {',
    '  line <- readLines(con, n = 1, warn = FALSE)',
    '  if (length(line) == 0) break',
    '  line <- trimws(line)',
    '  if (!nzchar(line)) next',
    '  msg <- tryCatch(fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)',
    '  if (!is.null(msg)) handle(msg)',
    '}'
  ))

  # Find Rscript
  rscript <- file.path(R.home("bin"), "Rscript")
  if (.Platform$OS.type == "windows") {
    # Try with .exe first, then without
    candidates <- c(
      paste0(rscript, ".exe"),
      rscript,
      file.path(R.home(), "bin", "Rscript.exe"),
      file.path(R.home(), "bin", "Rscript")
    )
    found <- candidates[file.exists(candidates)]
    if (length(found) == 0) {
      cli::cli_abort(c(
        "Cannot find Rscript executable.",
        "i" = "Tried: {.path {candidates}}"
      ))
    }
    rscript <- found[1]
  }
  rscript <- normalizePath(rscript, winslash = "/", mustWork = TRUE)

  list(
    script_path = normalizePath(script_path, winslash = "/", mustWork = FALSE),
    tools_path = normalizePath(tools_path, winslash = "/", mustWork = FALSE),
    rscript = rscript
  )
}


#' Build an MCP config JSON file for --additional-mcp-config
#'
#' Writes a temporary JSON file containing the mcpServers config that
#' can be passed to the CLI via `--additional-mcp-config @path`.
#' This bypasses the broken `mcpServers` parameter in `session/new`.
#'
#' @param tools Named list of tool definitions (from as_tool_def()).
#' @return A list with `config_path` (the JSON file), `script_path`,
#'   `tools_path`, and `rscript`.
#' @noRd
build_mcp_config <- function(tools, ipc_dir = NULL, backend = "copilot") {
  mcp <- build_mcp_script(tools, ipc_dir = ipc_dir, backend = backend)

  # Build R_LIBS from current .libPaths() -- ensures the CLI-spawned Rscript

  # subprocess finds the same libraries as the running R session, even when
  # R_LIBS_USER points to a broken or incomplete install.
  r_libs <- paste(.libPaths(), collapse = .Platform$path.sep)

  config <- list(
    mcpServers = list(
      `r-tools` = list(
        type = "stdio",
        command = mcp$rscript,
        args = list("--vanilla", mcp$script_path),
        env = list(R_LIBS = r_libs)
      )
    )
  )

  config_path <- tempfile("hal_mcp_config_", fileext = ".json")
  writeLines(
    jsonlite::toJSON(config, auto_unbox = TRUE, null = "null", pretty = TRUE),
    config_path
  )

  c(mcp, list(
    config_path = normalizePath(config_path, winslash = "/", mustWork = FALSE)
  ))
}
