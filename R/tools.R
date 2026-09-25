#' Convert a tool definition to hal's internal format
#'
#' Accepts either an `ellmer::ToolDef` object or a plain list with the
#' required fields.
#'
#' @param tool Tool definition -- an `ellmer::ToolDef` or a list with
#'   `name`, `description`, `fun`, and `arguments`/`parameters`.
#' @return A list with `name`, `description`, `fun`, and `schema`.
#' @noRd
as_tool_def <- function(tool) {
  # ellmer ToolDef (S7 class that inherits from function). ellmer >= 0.4 names
  # the class "ellmer::ToolDef"; older releases used "ToolDef". Both are
  # themselves callable, so the object doubles as `fun`.
  if (inherits(tool, "ellmer::ToolDef") || inherits(tool, "ToolDef")) {
    return(list(
      name = tool@name,
      description = tool@description,
      fun = tool,
      schema = list(
        type = "function",
        "function" = compact(list(
          name = tool@name,
          description = tool@description,
          parameters = tool_args_to_json(tool@arguments)
        ))
      )
    ))
  }

  # Plain list format
  if (is.list(tool)) {
    name <- tool$name %||% cli::cli_abort("Tool must have a {.arg name}.")
    desc <- tool$description %||% ""
    fun <- tool$fun %||% cli::cli_abort("Tool must have a {.arg fun}.")
    params <- tool$parameters %||% tool$arguments %||% list()

    return(list(
      name = name,
      description = desc,
      fun = fun,
      schema = list(
        type = "function",
        "function" = compact(list(
          name = name,
          description = desc,
          parameters = params
        ))
      )
    ))
  }

  cli::cli_abort("Unsupported tool type: {.cls {class(tool)}}")
}

#' Serialize a tool's return value to text for the model
#'
#' A character scalar passes through unchanged (already-formatted output such as
#' a markdown report). Everything else is JSON-encoded so structured returns
#' (data frames, lists) reach the model as readable data rather than the
#' column-deparse noise that `as.character()` produces on a data frame.
#'
#' @param out A tool function's return value.
#' @return A length-1 character string.
#' @noRd
tool_result_text <- function(out) {
  if (is.null(out)) return("(no output)")
  if (is.character(out) && length(out) == 1L && !is.na(out)) return(out)
  txt <- tryCatch(
    as.character(jsonlite::toJSON(
      out, dataframe = "rows", auto_unbox = TRUE,
      na = "null", null = "null", POSIXt = "ISO8601"
    )),
    error = function(e) NULL
  )
  if (!is.null(txt)) return(txt)
  paste(utils::capture.output(print(out)), collapse = "\n")
}

#' Convert an ellmer TypeObject (tool arguments) to JSON Schema
#'
#' Recursively lowers ellmer's S7 type tree to a JSON Schema list. Handles
#' ellmer >= 0.4 (`ellmer::TypeBasic` carrying `@type`, plus `TypeArray` /
#' `TypeObject` / `TypeEnum`) and falls back to the legacy 0.3 class names.
#'
#' @param args An ellmer `TypeObject` (S7).
#' @return A list representing JSON Schema for the parameters.
#' @noRd
tool_args_to_json <- function(args) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    return(if (is.list(args)) args else list())
  }
  tryCatch(
    .ellmer_type_to_schema(args),
    error = function(e) if (is.list(args)) args else list()
  )
}

#' Recursively convert one ellmer type to a JSON Schema node
#' @noRd
.ellmer_type_to_schema <- function(t) {
  cls <- class(t)[1]

  # Object: {type, properties, required}
  if (inherits(t, "ellmer::TypeObject") || identical(cls, "TypeObject")) {
    props <- t@properties
    nms <- names(props)
    req <- vapply(props, .ellmer_prop_required, logical(1))
    properties <- lapply(props, .ellmer_type_to_schema)
    names(properties) <- nms
    return(compact(list(
      type = "object",
      properties = properties,
      required = if (any(req)) as.list(nms[req]) else NULL
    )))
  }

  # Array: {type, items}
  if (inherits(t, "ellmer::TypeArray") || identical(cls, "TypeArray")) {
    return(compact(list(
      type = "array",
      description = .ellmer_prop_desc(t),
      items = .ellmer_type_to_schema(t@items)
    )))
  }

  # Enum: a string constrained to a set of values
  if (inherits(t, "ellmer::TypeEnum") || identical(cls, "TypeEnum")) {
    return(compact(list(
      type = "string",
      description = .ellmer_prop_desc(t),
      enum = as.list(t@values)
    )))
  }

  # Basic scalar (string / integer / number / boolean)
  compact(list(
    type = .ellmer_basic_type(t),
    description = .ellmer_prop_desc(t)
  ))
}

#' @noRd
.ellmer_basic_type <- function(t) {
  ty <- tryCatch(t@type, error = function(e) NULL)  # ellmer >= 0.4 TypeBasic
  if (!is.null(ty) && nzchar(ty)) return(ty)
  switch(class(t)[1],                               # legacy ellmer 0.3 fallback
    TypeString = "string",
    TypeNumber = "number",
    TypeInteger = "integer",
    TypeBoolean = "boolean",
    TypeEnum = "string",
    TypeArray = "array",
    TypeObject = "object",
    "string"
  )
}

#' @noRd
.ellmer_prop_desc <- function(t) {
  d <- tryCatch(t@description, error = function(e) NULL)
  if (is.null(d) || !length(d) || !nzchar(d)) NULL else d
}

#' @noRd
.ellmer_prop_required <- function(t) {
  isTRUE(tryCatch(t@required, error = function(e) TRUE))
}
