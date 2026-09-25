# ==============================================================================
# hal Tool Builder -- Convert R functions to MCP tool definitions
# ==============================================================================
#
# Ported from HAL's HAL_tool_register. Key difference: goes directly to
# JSON Schema instead of through ellmer's type_*() indirection.

# ------------------------------------------------------------------------------
# Type inference from formals
# ------------------------------------------------------------------------------

#' Infer JSON Schema type from a default value expression
#'
#' @param default_expr A formal default expression (from `formals()`).
#' @return Character string: "string", "number", "integer", or "boolean".
#' @keywords internal
#' @noRd
.hal_infer_type <- function(default_expr) {
  if (is.null(default_expr) || identical(default_expr, quote(expr = ))) {
    return("string")
  }
  txt <- deparse(default_expr, width.cutoff = 200)
  if (grepl("^[0-9]+L$", txt)) return("integer")
  if (grepl("^[0-9]+(\\.[0-9]+)?$", txt)) return("number")
  if (tolower(txt) %in% c("true", "false")) return("boolean")
  "string"
}

#' Build JSON Schema properties from a function's formals
#'
#' @param fn A function.
#' @param types Optional named list overriding type strings per argument.
#' @return A list with `properties` and `required` suitable for JSON Schema.
#' @keywords internal
#' @noRd
.hal_schema_from_formals <- function(fn, types = NULL) {
  fmls <- formals(fn)
  if (length(fmls) == 0L) {
    return(list(type = "object", properties = named_list()))
  }

  properties <- list()
  required <- character()

  for (nm in names(fmls)) {
    # User-supplied type override
    type_str <- if (!is.null(types) && !is.null(types[[nm]])) {
      types[[nm]]
    } else {
      .hal_infer_type(fmls[[nm]])
    }

    properties[[nm]] <- list(type = type_str)

    # Required if no default
    if (identical(fmls[[nm]], quote(expr = ))) {
      required <- c(required, nm)
    }
  }

  compact(list(
    type = "object",
    properties = properties,
    required = if (length(required)) as.list(required) else NULL
  ))
}

# ------------------------------------------------------------------------------
# Public API: hal_tool()
# ------------------------------------------------------------------------------

#' Create a Tool Definition from an R Function
#'
#' Converts an R function into a hal tool definition suitable for
#' `$register_tool()`. Infers parameter types from function formals;
#' override with the `types` argument for non-string parameters.
#'
#' @param fn The R function to expose as a tool.
#' @param name Tool name (defaults to the symbol name of `fn`).
#' @param description Description shown to the LLM (when to call this tool).
#' @param types Optional named list mapping argument names to JSON Schema type
#'   strings: `"string"`, `"number"`, `"integer"`, `"boolean"`, `"array"`,
#'   `"object"`.
#'
#' @return A list with `name`, `description`, `fun`, and `parameters` -- ready
#'   for `chat$register_tool()`.
#'
#' @examples
#' my_sum <- function(x, y) x + y
#' tool <- hal_tool(my_sum, "add_numbers",
#'   description = "Add two numbers",
#'   types = list(x = "number", y = "number")
#' )
#' str(tool)
#' \dontrun{
#' # register on the active session so the model can call it
#' hal_register_tools(list(tool))
#' }
#'
#' @export
hal_tool <- function(fn, name = NULL, description = NULL, types = NULL) {
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function.")
  }

  name <- name %||% deparse(substitute(fn))
  description <- description %||% paste("R tool:", name)
  schema <- .hal_schema_from_formals(fn, types = types)

  # Warn about required params typed as string by default
  fmls <- formals(fn)
  missing_hint <- vapply(names(fmls), function(nm) {
    user_override <- !is.null(types) && !is.null(types[[nm]])
    is_required <- identical(fmls[[nm]], quote(expr = ))
    !user_override && is_required
  }, logical(1))

  if (any(missing_hint)) {
    bad <- names(fmls)[missing_hint]
    bad_str <- paste(bad, collapse = ", ")
    cli::cli_warn(c(
      "!" = paste0("Parameter(s) ", bad_str, " in '", name, "' typed as string (no default)."),
      "i" = "If they expect numbers or booleans, specify {.arg types} explicitly."
    ))
  }

  list(
    name = name,
    description = description,
    fun = fn,
    parameters = schema
  )
}

# ------------------------------------------------------------------------------
# Convenience: register from a function directly
# ------------------------------------------------------------------------------

#' Register an R Function as a Copilot Tool
#'
#' Shorthand that builds a tool definition and registers it on the active
#' session's chat object.
#'
#' @inheritParams hal_tool
#'
#' @return Invisibly returns the tool definition.
#'
#' @examples
#' \dontrun{
#' get_time <- function(tz = "UTC") format(Sys.time(), tz = tz)
#' hal_register_tool(get_time,
#'   description = "Current time in a given timezone"
#' )
#' hal("what time is it in Tokyo?")
#' }
#' @export
hal_register_tool <- function(fn, name = NULL, description = NULL,
                                  types = NULL) {
  tool_def <- hal_tool(fn, name = name, description = description,
                           types = types)

  session <- .hal_ensure_session()
  session$chat$register_tool(tool_def)
  session$tool_names <- unique(c(session$tool_names, tool_def$name))

  invisible(tool_def)
}

#' Register Multiple Tools on the Active Session
#'
#' Registers a list of tools on the active `hal()` session so the model can
#' call them. Each element may be an `ellmer::ToolDef` (e.g. from
#' `daisy::daisy_tools()`) or a plain hal tool list with `name` / `description`
#' / `fun` / `parameters`. Unlike [hal_register_tool()], which builds a tool
#' from a bare function's formals, this passes each element through the chat's
#' tool normalizer, so pre-built `ToolDef`s keep their declared argument schema.
#'
#' @param tools A list of tool definitions (`ellmer::ToolDef` objects or hal
#'   tool lists).
#'
#' @return Invisibly, the number of tools registered.
#' @seealso [hal_register_tool()], [hal_register_tool_specs()]
#'
#' @examples
#' \dontrun{
#' tools <- list(
#'   hal_tool(function(x, y) x + y, "add",
#'     description = "Add two numbers",
#'     types = list(x = "number", y = "number")
#'   ),
#'   hal_tool(function(path) readLines(path), "read_file",
#'     description = "Read a text file"
#'   )
#' )
#' hal_register_tools(tools)
#' }
#' @export
hal_register_tools <- function(tools) {
  if (!is.list(tools)) {
    cli::cli_abort("{.arg tools} must be a list of tool definitions.")
  }
  session <- .hal_ensure_session()
  session$chat$register_tools(tools)
  nms <- vapply(tools, .hal_tool_name, character(1))
  session$tool_names <- unique(c(session$tool_names, nms[!is.na(nms)]))
  cli::cli_alert_success("Registered {length(tools)} tool{?s}.")
  invisible(length(tools))
}

#' Name of a tool definition (ellmer ToolDef or plain list)
#' @keywords internal
#' @noRd
.hal_tool_name <- function(tool) {
  if (inherits(tool, "ellmer::ToolDef") || inherits(tool, "ToolDef")) {
    return(tool@name)
  }
  tool$name %||% tool$id %||% NA_character_
}

#' List Registered Tools
#'
#' @return A character vector of tool names registered on the active session.
#'
#' @examples
#' hal_tools()  # character(0) when no session is active
#' @export
hal_tools <- function() {
  session <- .hal_get_session()
  if (is.null(session$chat)) {
    cli::cli_alert_info("No active session.")
    return(character())
  }
  session$tool_names %||% character()
}

# ------------------------------------------------------------------------------
# Bulk registration from package
# ------------------------------------------------------------------------------

#' Register Functions from an R Package as Tools
#'
#' Discovers exported functions from a package and registers them as
#' LLM-callable tools.
#'
#' @param pkg Character; package name.
#' @param fns Character vector of function names. If NULL, registers all
#'   exported functions.
#' @param exclude Character vector of function names to skip.
#' @param prefix Optional prefix for tool names (e.g., `"dplyr_"`).
#'
#' @return Invisibly returns a list of registered tool definitions.
#'
#' @examples
#' \dontrun{
#' # expose a few dplyr verbs to the model
#' hal_register_package_tools("dplyr",
#'   fns = c("filter", "select", "arrange"), prefix = "dplyr_"
#' )
#' }
#' @export
hal_register_package_tools <- function(pkg, fns = NULL, exclude = NULL,
                                           prefix = NULL) {
  namespace <- tryCatch(
    loadNamespace(pkg),
    error = function(e) cli::cli_abort("Cannot load package {.pkg {pkg}}: {e$message}")
  )

  if (is.null(fns)) {
    fns <- getNamespaceExports(pkg)
  }
  if (!is.null(exclude)) {
    fns <- setdiff(fns, exclude)
  }

  registered <- list()
  for (fn_name in fns) {
    fn <- tryCatch(
      get(fn_name, envir = namespace, mode = "function"),
      error = function(e) NULL
    )
    if (is.null(fn)) next

    tool_name <- if (!is.null(prefix)) paste0(prefix, fn_name) else fn_name
    tool_def <- tryCatch(
      hal_register_tool(
        fn, name = tool_name,
        description = sprintf("Function %s from package %s", fn_name, pkg)
      ),
      error = function(e) {
        cli::cli_warn("Skipping {.fn {fn_name}}: {e$message}")
        NULL
      }
    )
    if (!is.null(tool_def)) registered[[fn_name]] <- tool_def
  }

  cli::cli_alert_success(
    "Registered {length(registered)} tool{?s} from {.pkg {pkg}}."
  )
  invisible(registered)
}

# ------------------------------------------------------------------------------
# Bulk registration from tool specs (e.g., winston::timelog_tool_specs())
# ------------------------------------------------------------------------------

#' Register Tool Specs
#'
#' Registers a list of tool specifications (as returned by e.g.
#' `winston::timelog_tool_specs()`) on the active session.
#'
#' Each spec must be a list with at minimum `name`, `description`, and
#' `handler` (a function). Optional: `parameters` (JSON Schema).
#'
#' @param specs A list of tool spec lists.
#' @return Invisibly returns the number of tools registered.
#'
#' @examples
#' \dontrun{
#' specs <- list(
#'   list(
#'     name = "row_count",
#'     description = "Count the rows of a data frame in the global environment",
#'     handler = function(name) nrow(get(name, envir = globalenv()))
#'   )
#' )
#' hal_register_tool_specs(specs)
#' }
#' @export
hal_register_tool_specs <- function(specs) {
  if (!is.list(specs)) {
    cli::cli_abort("{.arg specs} must be a list of tool specifications.")
  }
  session <- .hal_ensure_session()

  count <- 0L
  for (spec in specs) {
    tool_def <- list(
      name = spec$name %||% spec$id,
      description = spec$description %||% "",
      fun = spec$handler %||% spec$fun,
      parameters = spec$parameters %||% list(type = "object", properties = named_list())
    )
    if (is.null(tool_def$fun)) {
      cli::cli_warn("Skipping tool spec without handler: {.val {tool_def$name}}")
      next
    }
    session$chat$register_tool(tool_def)
    session$tool_names <- unique(c(session$tool_names, tool_def$name))
    count <- count + 1L
  }

  cli::cli_alert_success("Registered {count} tool spec{?s}.")
  invisible(count)
}
