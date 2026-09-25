# ==============================================================================
# hal_ask / hal_do -- Pipe-friendly verbs for data-aware LLM interaction
# ==============================================================================
#
# hal_ask() -- analysis verb (pipe + standalone, disposable session)
#   mtcars |> hal_ask("what patterns do you see?")
#   hal_ask("what does the pipe operator do?")
#
# hal_do() -- transformation / code gen verb (pipe + standalone, disposable session)
#   mtcars |> hal_do("normalize all numeric columns") |> head()
#   hal_do("write a function to normalize columns")

# ==============================================================================
# hal_ask -- Analysis verb
# ==============================================================================

#' Ask Copilot a Question -- Pipe and Standalone Modes
#'
#' **Pipe mode**: a pipe-terminal verb that sends a prompt to Copilot augmented
#' with the piped data context. The piped object is described via `str()` and
#' attached to the prompt. Returns `.data` invisibly (pipe passthrough).
#'
#' **Standalone mode**: called directly with a string prompt to ask a question
#' without data context. Returns the response text invisibly.
#'
#' Spawns a disposable session -- does NOT pollute the main `hal()` conversation.
#'
#' @param .data In pipe mode: an R object (data frame, model, etc.) piped in.
#'   In standalone mode: the first prompt string.
#' @param ... Character strings forming the prompt.
#'
#' @return **Pipe mode:** invisibly returns `.data` (pipe passthrough).
#'   **Standalone:** invisibly returns the response text.
#'
#' @examples
#' \dontrun{
#' # Pipe mode (data-aware analysis)
#' mtcars |> hal_ask("what are the key patterns?")
#' lm(mpg ~ wt, data = mtcars) |> hal_ask("interpret these coefficients")
#'
#' # Standalone mode (general question)
#' hal_ask("what does the pipe operator do in R?")
#' hal_ask("explain", "the difference between <- and =")
#' }
#'
#' @export
hal_ask <- function(.data, ...) {
  # Detect standalone vs pipe mode
  raw_data_expr <- match.call()$.data
  standalone <- is.character(.data) && is.character(raw_data_expr)

  # Build prompt
  dots <- list(...)
  if (standalone) {
    if (!all(vapply(dots, is.character, logical(1)))) {
      cli::cli_abort("All prompt arguments must be character strings.")
    }
    user_question <- paste(c(.data, unlist(dots)), collapse = " ")
    augmented_prompt <- user_question
  } else {
    if (!length(dots)) {
      cli::cli_abort("At least one prompt string is required.")
    }
    if (!all(vapply(dots, is.character, logical(1)))) {
      cli::cli_abort("All prompt arguments must be character strings.")
    }
    user_question <- paste(unlist(dots), collapse = " ")

    # Capture piped code expression
    code_text <- if (!is.null(raw_data_expr)) {
      deparse(raw_data_expr, width.cutoff = 500L)
    } else {
      NULL
    }

    # Build data description
    data_desc <- tryCatch(
      paste(utils::capture.output(utils::str(.data, max.level = 2L)),
            collapse = "\n"),
      error = function(e) "<unable to describe data>"
    )

    sections <- c(
      paste(
        "Answer the following question using ONLY the data context provided below.",
        "Do NOT use your file, shell, or code tools -- all the information you need",
        "is included in this prompt.\n"
      ),
      user_question
    )
    if (!is.null(code_text)) {
      code_str <- paste(code_text, collapse = "\n")
      sections <- c(sections, paste0("\n\n## Piped Code\n```r\n", code_str, "\n```"))
    }
    sections <- c(sections, paste0("\n\n## Data Context\n", data_desc))
    augmented_prompt <- paste(sections, collapse = "")
  }

  # Use existing client with a temporary session (fast, no new process)
  session <- .hal_ensure_session()
  client <- session$chat$get_client()

  # Swap to a disposable session for history isolation
  saved_sid <- tryCatch(
    client$swap_session(),
    error = function(e) {
      cli::cli_warn(
        "hal_ask: failed to create disposable session: {e$message}",
        class = c("hal_ask_warning", "hal_warning")
      )
      NULL
    }
  )
  if (is.null(saved_sid) && !is.null(client$get_session_id())) {
    # swap_session failed but client has a session -- something went wrong
    return(if (standalone) invisible(NULL) else invisible(.data))
  }

  # Send prompt on the disposable session, then restore
  response_text <- tryCatch(
    client$prompt(augmented_prompt),
    error = function(e) {
      cli::cli_warn(
        "hal_ask: LLM request failed: {e$message}",
        class = c("hal_ask_warning", "hal_warning")
      )
      NULL
    }
  )

  # Always restore the original session
  if (!is.null(saved_sid)) {
    client$restore_session(saved_sid)
  }

  if (is.null(response_text)) {
    return(if (standalone) invisible(NULL) else invisible(.data))
  }

  # prompt() returns a hal_response object, extract text
  resp_text <- if (is.character(response_text)) {
    response_text
  } else {
    response_text$text
  }

  # Display with formatting
  .hal_display_response(resp_text)

  if (standalone) invisible(resp_text) else invisible(.data)
}

# ==============================================================================
# hal_do -- Transformation / code generation verb
# ==============================================================================

#' Code Generation via LLM -- Pipe and Standalone Modes
#'
#' **Pipe mode**: a mid-pipe transformation verb that generates R code,
#' executes it, and returns the result.
#'
#' **Standalone mode**: called directly with a string prompt to generate
#' and execute R code in the caller's environment.
#'
#' Spawns a disposable session -- does NOT pollute the main conversation.
#' Governance controls (denylist, credential scanner, timeout) are enforced.
#'
#' @param .data In pipe mode: an R object piped in. In standalone mode: the
#'   first prompt string.
#' @param ... Character strings describing the transformation/task.
#' @param .model Model to use for code generation (default: session model).
#' @param .retries Integer; max retry attempts when generated code fails to
#'   parse or execute (default: 2, configurable via `hal.do_retries` option).
#'   Set to 0 to disable retries. The retry re-prompts the same disposable
#'   session with the error message, so the model can self-correct.
#' @param .verify Logical; in pipe mode with a data-frame result, compare the
#'   input and output and display a one-line structural report (row/column
#'   deltas, class changes, introduced NAs). The full report is attached as
#'   `attr(result, "hal_verify")`. Report-only: it never affects retries or
#'   the returned values, but suspicious patterns (output identical to
#'   input, 0-row output) raise a classed `hal_do_warning`. Default TRUE
#'   (configurable via the `hal.verify` option).
#'
#' @return **Pipe mode:** transformed data. **Standalone:** result of generated
#'   code (invisible).
#'
#' @section Failure behavior:
#' In an **interactive** session, a failure (after retries) warns and returns
#' `.data` unchanged (pipe) or `invisible(NULL)` (standalone) -- forgiving
#' for console exploration. In **non-interactive** contexts (scripts, R
#' Markdown, `targets`), it aborts instead: a pipeline silently continuing
#' with untransformed data is worse than an error. Override either way with
#' `options(hal.do_on_fail = "warn")` or `"abort"`. All failures signal
#' classed conditions (`hal_do_error` / `hal_do_warning`), so you can
#' `tryCatch(..., hal_do_error = function(e) ...)`.
#'
#' @examples
#' \dontrun{
#' # Pipe mode
#' mtcars |> hal_do("keep only cars with mpg > 25")
#' iris |> hal_do("add a column for petal area") |> head()
#'
#' # Standalone mode
#' hal_do("write a function called greet that takes a name")
#' greet("world")
#'
#' # Disable retries
#' mtcars |> hal_do("normalize mpg", .retries = 0)
#' }
#'
#' @export
hal_do <- function(.data, ..., .model = NULL, .retries = NULL, .verify = NULL) {
  caller_env <- parent.frame()

  # Detect standalone vs pipe mode
  raw_data_expr <- match.call()$.data
  standalone <- is.character(.data) && is.character(raw_data_expr)

  # Build prompt
  dots <- list(...)
  if (standalone) {
    if (!all(vapply(dots, is.character, logical(1)))) {
      cli::cli_abort("All prompt arguments must be character strings.")
    }
    user_request <- paste(c(.data, unlist(dots)), collapse = " ")
  } else {
    if (!length(dots)) {
      cli::cli_abort("At least one transformation description is required.")
    }
    if (!all(vapply(dots, is.character, logical(1)))) {
      cli::cli_abort("All prompt arguments must be character strings.")
    }
    user_request <- paste(unlist(dots), collapse = " ")
  }

  # Build context
  if (standalone) {
    env_desc <- .hal_describe_env_layered(caller_env)
    sections <- c(user_request)
    if (nzchar(env_desc)) {
      sections <- c(sections, paste0("\n\n## Available Objects\n", env_desc))
    }
    prompt <- paste(sections, collapse = "")
    sys_prompt <- .hal_do_standalone_system_prompt()
  } else {
    code_text <- if (!is.null(raw_data_expr)) {
      deparse(raw_data_expr, width.cutoff = 500L)
    } else {
      NULL
    }

    data_desc <- tryCatch(
      paste(utils::capture.output(utils::str(.data, max.level = 2L)),
            collapse = "\n"),
      error = function(e) "<unable to describe data>"
    )

    sections <- c(user_request)
    if (!is.null(code_text)) {
      code_str <- paste(code_text, collapse = "\n")
      sections <- c(sections,
                    paste0("\n\n## Piped Code\n```r\n", code_str, "\n```"))
    }
    sections <- c(sections, paste0("\n\n## Data Description\n", data_desc))
    prompt <- paste(sections, collapse = "")
    sys_prompt <- .hal_do_pipe_system_prompt()
  }

  # Governance: scan prompt
  prompt <- .hal_scan_outbound(prompt)

  # Spawn disposable worker
  fail_return <- if (standalone) invisible(NULL) else .data
  chat <- tryCatch(
    HalChat$new(
      model = .model %||% getOption("hal.default_model"),
      system_prompt = sys_prompt,
      echo = "none",
      quiet = TRUE
    ),
    error = function(e) {
      cli::cli_alert_danger("hal_do: failed to create worker: {e$message}")
      NULL
    }
  )
  if (is.null(chat)) {
    return(.hal_do_fail("hal_do: failed to create worker.", fail_return))
  }

  # Get response
  err_msg <- NULL
  response <- tryCatch(
    chat$chat(prompt, timeout = 120),
    error = function(e) {
      err_msg <<- e$message
      NULL
    }
  )
  if (is.null(response) || !nzchar(trimws(response))) {
    msg <- if (!is.null(err_msg)) {
      paste0("hal_do: LLM request failed: ", err_msg)
    } else {
      "hal_do: LLM returned empty response."
    }
    return(.hal_do_fail(msg, fail_return))
  }

  # Retry config
  max_retries <- as.integer(.retries %||% getOption("hal.do_retries", 2L))

  # Attempt loop: extract, parse, execute -- retry on failure
  final_code <- NULL
  result <- NULL

  for (attempt in seq_len(max_retries + 1L)) {
    # Extract code
    code <- .hal_extract_code(response)
    if (!nzchar(code)) {
      if (attempt <= max_retries) {
        cli::cli_alert_warning(
          "Retry {attempt}/{max_retries}: no code extracted"
        )
        response <- .hal_do_retry(chat, "No valid R code in your response.", "")
        if (is.null(response)) break
        next
      }
      return(.hal_do_fail(
        "hal_do: could not extract code from response.", fail_return
      ))
    }

    # Show generated code
    .hal_show_generated_code(code)

    # Governance: denylist check -- never retry (security boundary)
    block_msg <- .hal_check_eval_denylist(code)
    if (!is.null(block_msg)) {
      return(.hal_do_fail(paste0("hal_do: ", block_msg), fail_return))
    }

    # Parse
    parsed <- tryCatch(parse(text = code), error = identity)
    if (inherits(parsed, "error")) {
      if (attempt <= max_retries) {
        cli::cli_alert_warning(
          "Retry {attempt}/{max_retries}: parse error"
        )
        response <- .hal_do_retry(
          chat, paste("Parse error:", conditionMessage(parsed)), code
        )
        if (is.null(response)) break
        next
      }
      return(.hal_do_fail(
        paste0("hal_do: parse error: ", conditionMessage(parsed)), fail_return
      ))
    }

    # Execute
    exec_result <- if (standalone) {
      tryCatch(
        .hal_eval_with_timeout(parsed, envir = caller_env),
        error = identity
      )
    } else {
      exec_env <- new.env(parent = parent.env(globalenv()))
      exec_env$.data <- .data
      tryCatch(
        .hal_eval_with_timeout(parsed, envir = exec_env),
        error = identity
      )
    }

    if (inherits(exec_result, "error")) {
      if (attempt <= max_retries) {
        cli::cli_alert_warning(
          "Retry {attempt}/{max_retries}: {conditionMessage(exec_result)}"
        )
        response <- .hal_do_retry(
          chat, paste("Execution error:", conditionMessage(exec_result)), code
        )
        if (is.null(response)) break
        next
      }
      return(.hal_do_fail(
        paste0("hal_do: execution error: ", conditionMessage(exec_result)),
        fail_return
      ))
    }

    # Success
    if (attempt > 1L) cli::cli_alert_success("Retry succeeded.")
    result <- exec_result
    final_code <- code
    break
  }

  if (is.null(result) || length(result) == 0L) {
    return(.hal_do_fail("hal_do: execution produced empty result.", fail_return))
  }

  # Verification report (report-only: never feeds the retry loop above)
  if (!standalone) {
    result <- .hal_do_verify(.data, result, .verify)
  }

  # Edit-in-place (uses final working code)
  .hal_edit_in_place(final_code)

  if (standalone) invisible(result) else result
}

# ==============================================================================
# hal_do helpers
# ==============================================================================
# Note: System prompts and retry prompt builder are in R/prompts.R

#' Terminal failure handler for hal_do
#'
#' Interactive sessions get a warning and the unchanged `.data` back --
#' forgiving for console exploration. Non-interactive contexts (scripts,
#' Rmd, targets) abort instead: a pipeline silently continuing with
#' untransformed data is worse than an error. Override either way with
#' `options(hal.do_on_fail = "abort")` or `"warn"`.
#'
#' Both paths signal classed conditions (`hal_do_error` / `hal_do_warning`,
#' each inheriting from `hal_error` / `hal_warning`) so programmatic callers
#' can handle hal failures specifically.
#'
#' @param msg Message for the condition.
#' @param fail_return Value to return on the warn path.
#' @return `fail_return` (warn path); never returns on the abort path.
#' @keywords internal
#' @noRd
.hal_do_fail <- function(msg, fail_return) {
  on_fail <- getOption(
    "hal.do_on_fail",
    if (interactive()) "warn" else "abort"
  )
  if (identical(on_fail, "abort")) {
    cli::cli_abort(msg, class = c("hal_do_error", "hal_error"))
  }
  cli::cli_warn(msg, class = c("hal_do_warning", "hal_warning"))
  fail_return
}

#' Send a retry prompt to a disposable hal_do worker
#' @keywords internal
#' @noRd
.hal_do_retry <- function(chat, error_msg, code) {
  prompt <- .hal_do_retry_prompt(error_msg, code)
  response <- tryCatch(
    chat$chat(prompt, timeout = 120),
    error = function(e) {
      cli::cli_warn("hal_do: retry request failed: {e$message}")
      NULL
    }
  )
  if (is.null(response) || !nzchar(trimws(response))) return(NULL)
  response
}

#' English function words that should not trigger env injection by name alone
#'
#' A prompt like "make a plot of the data" tokenizes to several words that
#' could collide with object names. For unambiguous *function words* (the,
#' a, is, for, ...) a name-only match is almost certainly prose, so it is
#' ignored unless the prompt references the object deliberately (see
#' `.hal_detect_env_refs`). Contested data-science nouns (data, model, fit,
#' plot, ...) are deliberately NOT listed: when a user has an object called
#' `data` and says "the data", they usually mean it -- a false negative
#' (missing context) costs more than a false positive (a small snapshot
#' plus an info alert).
#'
#' @keywords internal
#' @noRd
.HAL_ENV_REF_STOPWORDS <- c(
  "a", "about", "add", "after", "all", "an", "and", "any", "are", "as",
  "at", "be", "before", "but", "by", "can", "do", "does", "down", "drop",
  "each", "every", "find", "first", "for", "from", "get", "give", "has",
  "have", "here", "how", "i", "if", "in", "into", "is", "it", "its",
  "keep", "last", "make", "me", "more", "most", "my", "new", "next",
  "no", "not", "now", "of", "on", "one", "only", "or", "other", "out",
  "over", "read", "remove", "run", "same", "set", "show", "so", "some",
  "tell", "that", "the", "then", "there", "this", "to", "two", "up",
  "us", "use", "was", "what", "when", "where", "which", "will", "with",
  "write", "you", "your"
)

#' Detect environment object references in a prompt
#'
#' Tokenizes the prompt into R-style identifiers and returns the names
#' of caller-environment objects that appear as tokens. Used by `hal()`
#' to decide whether to inject an env snapshot: inject only when the user
#' actually mentions an object that exists.
#'
#' Matches that collide with common English function words (see
#' `.HAL_ENV_REF_STOPWORDS`) are kept only when the prompt references the
#' object *deliberately*: backtick-quoted (`` `on` ``), subset (`on$x`,
#' `on[`), or called (`on(`).
#'
#' @param prompt Character scalar, the user prompt.
#' @param env An environment to check against.
#' @return Character vector of matched object names (possibly empty).
#' @keywords internal
#' @noRd
.hal_detect_env_refs <- function(prompt, env) {
  nms <- ls(envir = env)
  if (!length(nms) || !length(prompt) || !nzchar(prompt)) return(character())
  tokens <- regmatches(
    prompt,
    gregexpr("[A-Za-z_.][A-Za-z0-9_.]*", prompt, perl = TRUE)
  )[[1]]
  if (!length(tokens)) return(character())
  matched <- intersect(nms, unique(tokens))
  if (!length(matched)) return(character())

  contested <- tolower(matched) %in% .HAL_ENV_REF_STOPWORDS
  if (!any(contested)) return(matched)

  # Rescue stopword collisions only on a deliberate reference:
  # `name` (backticks), name$..., name[..., or name(...)
  deliberate <- vapply(matched, function(nm) {
    esc <- gsub(".", "\\.", nm, fixed = TRUE)
    grepl(
      paste0("`", esc, "`|(^|[^A-Za-z0-9_.])", esc, "\\s*[$([]"),
      prompt
    )
  }, logical(1))

  matched[!contested | deliberate]
}

#' Brief environment summary -- names and types only
#'
#' Lists objects with their class. The model can use eval_r to inspect
#' anything it finds interesting. No need to pre-describe details.
#'
#' @param env An environment to describe.
#' @param max_objects Maximum objects to list.
#' @return Character string.
#' @keywords internal
#' @noRd
.hal_describe_env_layered <- function(env, max_objects = 30L) {
  nms <- ls(envir = env)
  if (length(nms) == 0L) return("")

  truncated <- length(nms) > max_objects
  nms <- head(nms, max_objects)

  lines <- vapply(nms, function(nm) {
    obj <- tryCatch(get(nm, envir = env), error = function(e) NULL)
    if (is.null(obj)) return(paste0(nm, ": <inaccessible>"))

    cls <- class(obj)[1L]
    if (is.data.frame(obj)) {
      paste0(nm, ": data.frame [", nrow(obj), " x ", ncol(obj), "]")
    } else if (is.function(obj)) {
      paste0(nm, ": function")
    } else if (is.atomic(obj) && length(obj) == 1L) {
      paste0(nm, ": ", cls)
    } else {
      paste0(nm, ": ", cls, " [", length(obj), "]")
    }
  }, character(1L))

  if (truncated) {
    lines <- c(lines, paste0("... and ", length(ls(envir = env)) - max_objects, " more"))
  }

  paste(lines, collapse = ", ")
}

#' Compact summary of objects in an environment
#' @keywords internal
#' @noRd
.hal_describe_env <- function(env, max_objects = 20L, max_chars = 3000L) {
  nms <- ls(envir = env)
  if (length(nms) == 0L) return("")

  truncated <- length(nms) > max_objects
  nms <- head(nms, max_objects)

  lines <- vapply(nms, function(nm) {
    obj <- tryCatch(get(nm, envir = env), error = function(e) NULL)
    if (is.null(obj)) return(paste0("- ", nm, ": <inaccessible>"))

    if (is.data.frame(obj)) {
      .hal_describe_df(nm, obj)
    } else if (is.function(obj)) {
      .hal_describe_fn(nm, obj)
    } else if (is.atomic(obj) && length(obj) == 1L) {
      paste0("- ", nm, ": ", class(obj)[1L], ": ",
             tryCatch(as.character(obj), error = function(e) "?"))
    } else {
      paste0("- ", nm, ": ", class(obj)[1L], " [length ", length(obj), "]")
    }
  }, character(1L))

  if (truncated) {
    lines <- c(lines, paste0("... and ", length(ls(envir = env)) - max_objects, " more"))
  }

  out <- paste(lines, collapse = "\n")
  if (nchar(out) > max_chars) {
    out <- paste0(substr(out, 1L, max_chars - 20L), "\n... (truncated)")
  }
  out
}

#' Describe a data frame with column names and types
#' @keywords internal
#' @noRd
.hal_describe_df <- function(nm, obj) {
  header <- paste0("- ", nm, ": data.frame [", nrow(obj), " x ", ncol(obj), "]")
  col_names <- names(obj)
  if (length(col_names) == 0L) return(header)

  # Column types: one word per column

  col_types <- vapply(obj, function(col) class(col)[1L], character(1L))
  col_desc <- paste0(col_names, " (", col_types, ")", collapse = ", ")

  # Cap column description at 500 chars for wide data frames
  if (nchar(col_desc) > 500L) {
    col_desc <- paste0(substr(col_desc, 1L, 480L), "...")
  }

  paste0(header, "\n  cols: ", col_desc)
}

#' Describe a function with signature and optionally its body
#' @keywords internal
#' @noRd
.hal_describe_fn <- function(nm, obj, max_body_lines = 5L) {
  args <- tryCatch(paste(names(formals(obj)), collapse = ", "),
                   error = function(e) "...")
  header <- paste0("- ", nm, ": function(", args, ")")

  # Include body for small functions
  fn_body <- tryCatch(deparse(body(obj)), error = function(e) NULL)
  if (is.null(fn_body) || length(fn_body) > max_body_lines) return(header)

  body_text <- paste(trimws(fn_body), collapse = " ")
  # Skip if the body is trivial (NULL or single primitive)
  if (body_text == "NULL" || !nzchar(body_text)) return(header)
  # Cap body at 200 chars
  if (nchar(body_text) > 200L) return(header)

  paste0(header, "\n  body: ", body_text)
}

#' Extract R code from an LLM response
#' @keywords internal
#' @noRd
.hal_extract_code <- function(response) {
  if (!is.character(response) || !nzchar(response)) return("")
  lines <- .hal_strip_fences(trimws(response))
  code <- paste(lines, collapse = "\n")
  # Strip inline backtick wrapping: `code here` -> code here
  code <- gsub("^`+\\s*", "", code)
  code <- gsub("\\s*`+$", "", code)
  code
}

#' Display generated code to the user
#' @keywords internal
#' @noRd
.hal_show_generated_code <- function(code) {
  if (!nzchar(code)) return(invisible(NULL))
  use_colors <- .hal_supports_color()
  if (use_colors) {
    cat("\033[1;90m", "\u2192", "\033[0m ", sep = "")
    cat("\033[1;37m", code, "\033[0m\n", sep = "")
  } else {
    cat("-> ", code, "\n", sep = "")
  }
  invisible(NULL)
}

# ==============================================================================
# Edit-in-place
# ==============================================================================

#' Replace hal_do() in the editor with generated code
#' @keywords internal
#' @noRd
.hal_edit_in_place <- function(code) {
  # Default to TRUE when running from an IDE script, FALSE from console
  default <- FALSE
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path) && !identical(ctx$id, "#console")) {
      default <- TRUE
    }
  }
  if (!isTRUE(getOption("hal.edit_in_place", default))) return(invisible(NULL))

  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    return(invisible(NULL))
  }

  ctx <- tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) {
      tryCatch(rstudioapi::getActiveDocumentContext(),
               error = function(e2) NULL)
    }
  )
  if (is.null(ctx)) return(invisible(NULL))
  if (!nzchar(ctx$path) || identical(ctx$id, "#console")) {
    return(invisible(NULL))
  }

  range <- .hal_find_do_call_range(ctx)
  if (is.null(range)) return(invisible(NULL))

  # Strip .data pipe prefix: .data |> fn(...) -> fn(...)
  clean <- sub("^\\s*\\.data\\s*\\|>\\s*", "", code)
  clean <- sub("^\\s*\\.data\\s*%>%\\s*", "", clean)
  # Strip .data as first argument: fn(.data, ...) -> fn(...)
  clean <- sub("(\\w+\\()\\s*\\.data\\s*,\\s*", "\\1", clean)
  rstudioapi::modifyRange(range, clean, id = ctx$id)
  invisible(NULL)
}

#' Find hal_do() call range in editor
#' @keywords internal
#' @noRd
.hal_find_do_call_range <- function(ctx) {
  contents <- ctx$contents
  n_lines <- length(contents)
  if (n_lines == 0L) return(NULL)

  cursor_row <- tryCatch(
    ctx$selection[[1L]]$range$start[[1L]],
    error = function(e) 1L
  )
  search_start <- max(1L, cursor_row - 1L)

  target_row <- NULL
  col_start <- NULL
  for (row in seq(search_start, 1L)) {
    line <- contents[row]
    # Skip comment lines
    if (grepl("^\\s*#", line)) next
    pos <- regexpr("hal_do\\(", line)
    if (pos > 0L) {
      # Make sure the match isn't inside a comment (after a #)
      before <- substr(line, 1L, pos - 1L)
      if (!grepl("#", before)) {
        target_row <- row
        col_start <- as.integer(pos)
        break
      }
    }
  }
  if (is.null(target_row)) return(NULL)

  paren_col <- col_start + nchar("hal_do")
  depth <- 0L
  in_string <- FALSE
  string_char <- ""

  for (row in seq(target_row, n_lines)) {
    line <- contents[row]
    start_col <- if (row == target_row) paren_col else 1L
    i <- start_col
    while (i <= nchar(line)) {
      ch <- substr(line, i, i)
      if (in_string) {
        if (ch == "\\" && i < nchar(line)) { i <- i + 2L; next }
        if (ch == string_char) in_string <- FALSE
      } else {
        if (ch == "\"" || ch == "'") { in_string <- TRUE; string_char <- ch }
        else if (ch == "(") { depth <- depth + 1L }
        else if (ch == ")") {
          depth <- depth - 1L
          if (depth == 0L) {
            start_pos <- rstudioapi::document_position(target_row, col_start)
            end_pos <- rstudioapi::document_position(row, i + 1L)
            return(rstudioapi::document_range(start_pos, end_pos))
          }
        }
      }
      i <- i + 1L
    }
  }
  NULL
}
