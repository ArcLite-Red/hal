# ==============================================================================
# hal_excel -- turn an Excel sheet into R: read the data + recreate the formulas
# ==============================================================================
#
# The whole point: hand back runnable R that REPLACES the spreadsheet. Read an
# .xlsx, treat non-formula columns as data, and translate each formula column --
# ONE consistent column formula, R-vector style -- into a tidyverse expression
# via the active hal backend. The output is a script: a line that reads the
# input columns from the workbook, plus a mutate() pipeline that recreates the
# formula columns. You replace the hal_excel() call with the printed code.
#
# Each translation is run and checked against the values Excel cached, so only
# columns that match Excel row-for-row go into the live pipeline; the rest are
# emitted as commented stubs to review.
#
# Additive: reuses hal_do's internal helpers (code extraction, denylist, timed
# eval, retry loop) without modifying them. openxlsx2 is Suggests, guarded here.
# (openxlsx2 not tidyxl: tidyxl segfaults parsing the theme of some Excel-saved
# files -- xlsxstyles::rgb_string. openxlsx2 reads formulas + cached values
# without that crash.)
#
# Assumption: each formula column holds one formula filled down the column
# (e.g. =A2*B2, =A3*B3, ...), i.e. a vectorised expression -- not a different
# formula per row. We translate the column's representative (first) formula.

# ------------------------------------------------------------------------------
# Public entry point
# ------------------------------------------------------------------------------

#' Turn an Excel sheet into R: code that reads the data and recreates the formulas
#'
#' Reads an `.xlsx`, treats columns without formulas as data, and translates
#' each formula column into a tidyverse expression via the active hal backend.
#' Returns a runnable R script -- a line that reads the input columns from the
#' workbook plus a `mutate()` pipeline that recreates the formula columns -- so
#' you can replace the spreadsheet (and the `hal_excel()` call) with the code.
#'
#' Works like [hal_do()] under the hood (disposable worker, code extraction,
#' denylist, timed eval, retry loop), with one addition: each translation is run
#' and checked against the values Excel cached, so only columns that match Excel
#' row-for-row go into the live pipeline; the rest are emitted as commented
#' stubs to review.
#'
#' Each formula column is assumed to hold one formula filled down the column
#' (a vectorised expression, e.g. `=A2*B2`), so its first formula is translated
#' once. A workbook with no cached values is still translated but reported
#' `unverifiable`.
#'
#' @param path Path to an `.xlsx` workbook.
#' @param sheet Sheet name, or 1-based index (default `1`).
#' @param model Optional model id; defaults to the session/configured model.
#' @param tolerance Numeric tolerance for float comparison (default
#'   `hal.xl_tolerance`, or `1e-9`).
#' @param retries Max self-correction attempts per column on mismatch (default
#'   `hal.xl_retries`, then `hal.do_retries`, then `2`).
#'
#' @return A `hal_excel_code` object (a character string of R code) that prints as
#'   the generated script. The per-column report is attached as
#'   `attr(result, "hal_excel")`. Save it with `writeLines(result, "out.R")`.
#'
#' @examples
#' \dontrun{
#' hal_excel("model.xlsx")                # prints the read + mutate script
#' code <- hal_excel("model.xlsx")
#' writeLines(code, "model.R")         # or paste it in place of the call
#' attr(code, "hal_excel")               # per-column verification report
#' }
#'
#' @export
hal_excel <- function(path, sheet = 1, model = NULL,
                   tolerance = NULL, retries = NULL) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg openxlsx2} is required to read Excel formulas.",
      "i" = "Install with {.code install.packages('openxlsx2')}."
    ))
  }
  if (!file.exists(path)) cli::cli_abort("File not found: {.path {path}}.")

  tol <- tolerance %||% getOption("hal.xl_tolerance", 1e-9)
  max_retries <- as.integer(
    retries %||% getOption("hal.xl_retries", getOption("hal.do_retries", 2L))
  )

  wb <- .hal_excel_read(path, sheet)
  if (length(wb$formula_cols) == 0L) {
    cli::cli_alert_info("No formula columns on sheet {.val {wb$sheet}}.")
    return(invisible(NULL))
  }

  meta <- do.call(rbind, lapply(wb$formula_cols, function(spec) {
    .hal_excel_translate_one(spec, wb$data, wb$letter_map, model, tol, max_retries)$row
  }))

  n_ok <- sum(meta$status == "verified")
  cli::cli_alert_success(
    "{n_ok}/{nrow(meta)} column{?s} verified against Excel's cached values."
  )

  code <- .hal_excel_assemble(path, wb$sheet, names(wb$data), meta)
  attr(code, "hal_excel") <- meta

  # Like hal_do: replace the hal_excel() call in the editor with the generated
  # code. Falls back to printing (the visible return) when there's no source
  # editor / rstudioapi.
  if (.hal_excel_edit_in_place(code)) {
    cli::cli_alert_success("Replaced the {.code hal_excel()} call with the generated code.")
    return(invisible(code))
  }
  code
}

# ------------------------------------------------------------------------------
# Read workbook (openxlsx2) -> input columns + formula-column specs
# ------------------------------------------------------------------------------
# Two passes over the sheet: values, and formulas (show_formula = TRUE returns
# the formula string for formula cells and the value otherwise). A column is a
# formula column wherever the two passes differ; its cached values come from
# the value pass -- that's the verification oracle.

#' @keywords internal
#' @noRd
.hal_excel_read <- function(path, sheet) {
  wb <- openxlsx2::wb_load(path)
  sheet_names <- unname(openxlsx2::wb_get_sheet_names(wb))
  sheet_name <- if (is.numeric(sheet)) {
    if (sheet < 1 || sheet > length(sheet_names)) {
      cli::cli_abort("Sheet index {sheet} out of range (1..{length(sheet_names)}).")
    }
    sheet_names[[as.integer(sheet)]]
  } else {
    if (!sheet %in% sheet_names) {
      cli::cli_abort(c("Sheet {.val {sheet}} not found.",
                       "i" = "Available: {.val {sheet_names}}."))
    }
    sheet
  }

  vals <- openxlsx2::wb_to_df(wb, sheet = sheet_name, col_names = TRUE)
  fmla <- openxlsx2::wb_to_df(wb, sheet = sheet_name, col_names = TRUE,
                              show_formula = TRUE)
  if (is.null(vals) || !ncol(vals)) {
    cli::cli_abort("Sheet {.val {sheet_name}} has no data.")
  }

  headers <- names(vals)
  letters_vec <- vapply(seq_along(headers), .hal_excel_col_letter, character(1))
  letter_map <- setNames(headers, letters_vec)  # letter -> column name

  input_cols <- list()
  formula_cols <- list()
  for (k in seq_along(headers)) {
    nm <- headers[k]
    vcol <- vals[[k]]
    fcol <- as.character(fmla[[k]])
    # formula cells: the show_formula pass differs from the value pass
    diff <- which(!is.na(fcol) & fcol != as.character(vcol))
    if (length(diff)) {
      # one consistent formula per column -> take the first
      formula_cols[[length(formula_cols) + 1L]] <- list(
        name = nm, letter = letters_vec[k],
        formula = paste0("=", sub("^=", "", fcol[diff[1]])),
        cached = vcol, dtype = .hal_excel_dtype(vcol)
      )
    } else {
      input_cols[[nm]] <- vcol
    }
  }

  data <- if (length(input_cols)) {
    as.data.frame(input_cols, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    data.frame()
  }

  list(sheet = sheet_name, data = data, headers = headers,
       formula_cols = formula_cols, letter_map = letter_map)
}

#' @keywords internal
#' @noRd
.hal_excel_col_letter <- function(n) {
  s <- ""
  while (n > 0) {
    r <- (n - 1L) %% 26L
    s <- paste0(LETTERS[r + 1L], s)
    n <- (n - 1L) %/% 26L
  }
  s
}

#' Map an R column vector to a comparison dtype label
#' @keywords internal
#' @noRd
.hal_excel_dtype <- function(v) {
  if (inherits(v, c("Date", "POSIXct"))) "date"
  else if (is.logical(v)) "logical"
  else if (is.numeric(v)) "numeric"
  else "character"
}

# ------------------------------------------------------------------------------
# Translate + verify one formula column
# ------------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.hal_excel_translate_one <- function(spec, data, letter_map, model, tol,
                                  max_retries) {
  n_total <- length(spec$cached)
  has_cache <- !all(is.na(spec$cached))

  chat <- tryCatch(
    HalChat$new(model = model %||% getOption("hal.default_model"),
                system_prompt = .hal_excel_system_prompt(letter_map, names(data)),
                echo = "none", quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(chat)) {
    return(list(row = .hal_excel_row(spec, NA_character_, "error", NA_integer_,
                                  n_total, "worker init failed"), values = NULL))
  }

  response <- tryCatch(
    chat$chat(.hal_scan_outbound(.hal_excel_user_prompt(spec, data)), timeout = 120),
    error = function(e) NULL
  )

  # No cached values: translate best-effort, can't verify.
  if (!has_cache) {
    code <- .hal_excel_clean_code(.hal_extract_code(response %||% ""))
    if (!nzchar(code)) {
      return(list(row = .hal_excel_row(spec, NA_character_, "error", NA_integer_,
                                    n_total, "no candidate"), values = NULL))
    }
    block <- .hal_check_eval_denylist(code)
    if (!is.null(block)) {
      return(list(row = .hal_excel_row(spec, code, "blocked", NA_integer_,
                                    n_total, block), values = NULL))
    }
    run <- .hal_excel_run(code, data, spec$name)
    if (!run$ok) {
      return(list(row = .hal_excel_row(spec, code, "error", NA_integer_,
                                    n_total, run$error), values = NULL))
    }
    return(list(row = .hal_excel_row(spec, code, "unverifiable", NA_integer_,
                                  n_total, "no cached values in workbook"),
                values = run$value))
  }

  best <- NULL
  for (attempt in seq_len(max_retries + 1L)) {
    if (is.null(response) || !nzchar(trimws(response))) break
    code <- .hal_excel_clean_code(.hal_extract_code(response))
    if (!nzchar(code)) {
      if (attempt <= max_retries) {
        response <- .hal_do_retry(chat, "No R code in your response.", "")
        next
      }
      break
    }
    block <- .hal_check_eval_denylist(code)
    if (!is.null(block)) {
      return(list(row = .hal_excel_row(spec, code, "blocked", NA_integer_,
                                    n_total, block), values = NULL))
    }

    run <- .hal_excel_run(code, data, spec$name)
    if (run$ok) {
      cmp <- .hal_excel_compare(run$value, spec$cached, spec$dtype, tol)
      if (isTRUE(cmp$pass)) {
        return(list(row = .hal_excel_row(spec, code, "verified", cmp$n_match,
                                      n_total, NULL), values = run$value))
      }
      best <- list(code = code, value = run$value, n_match = cmp$n_match,
                   note = "values did not match Excel")
      report <- .hal_excel_mismatch_report(cmp)
    } else {
      best <- list(code = code, value = NULL, n_match = 0L, note = run$error)
      report <- paste0("Your code failed to run: ", run$error,
                       ". Return one mutate() that reproduces the column.")
    }
    if (attempt <= max_retries) {
      response <- .hal_do_retry(chat, report, code)
    }
  }

  if (is.null(best)) {
    return(list(row = .hal_excel_row(spec, NA_character_, "error", NA_integer_,
                                  n_total, "no candidate"), values = NULL))
  }
  list(row = .hal_excel_row(spec, best$code, "unverified", best$n_match, n_total,
                         best$note), values = best$value)
}

#' Run a candidate `mutate()` against the data; return the produced column
#' @keywords internal
#' @noRd
.hal_excel_run <- function(code, data, name) {
  exec_env <- new.env(parent = parent.env(globalenv()))
  exec_env$.data <- data
  res <- tryCatch(
    .hal_eval_with_timeout(parse(text = paste0(".data |> ", code)), exec_env),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(ok = FALSE, error = conditionMessage(res), value = NULL))
  }
  if (!is.data.frame(res) || !name %in% names(res)) {
    return(list(ok = FALSE,
                error = paste0("result has no column '", name, "'"),
                value = NULL))
  }
  list(ok = TRUE, error = NULL, value = res[[name]])
}

#' @keywords internal
#' @noRd
.hal_excel_compare <- function(got, cached, dtype, tol) {
  n <- length(cached)
  if (length(got) != n) {
    return(list(pass = FALSE, n_match = 0L,
                error = sprintf("length mismatch (%d vs %d)", length(got), n),
                mism = NULL))
  }
  match_vec <- tryCatch(
    switch(dtype,
      numeric = mapply(function(a, b) {
        if (is.na(a) && is.na(b)) TRUE
        else if (is.na(a) || is.na(b)) FALSE
        else isTRUE(abs(a - b) <= tol)
      }, as.numeric(got), as.numeric(cached)),
      date = (!is.na(got) & !is.na(cached) &
                as.Date(got) == as.Date(cached)) | (is.na(got) & is.na(cached)),
      logical = (got == cached) | (is.na(got) & is.na(cached)),
      character = (trimws(as.character(got)) == as.character(cached)) |
                  (is.na(got) & is.na(cached)),
      (as.character(got) == as.character(cached)) | (is.na(got) & is.na(cached))
    ),
    error = function(e) rep(FALSE, n)
  )
  match_vec[is.na(match_vec)] <- FALSE
  n_match <- sum(match_vec)
  mism <- NULL
  if (n_match < n) {
    show <- head(which(!match_vec), 5L)
    mism <- data.frame(row = show + 1L, excel = as.character(cached[show]),
                       r = as.character(got[show]), stringsAsFactors = FALSE)
  }
  list(pass = (n_match == n), n_match = n_match, error = NULL, mism = mism)
}

# ------------------------------------------------------------------------------
# Prompts
# ------------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.hal_excel_system_prompt <- function(letter_map, data_cols) {
  map_str <- paste(sprintf("%s=%s", names(letter_map), letter_map),
                   collapse = ", ")
  cols_str <- if (length(data_cols)) paste(data_cols, collapse = ", ") else "(none)"
  paste0(
    "You translate ONE Excel formula column into ONE tidyverse expression.\n",
    "Return ONLY R code: a single `mutate(<name> = ...)` and nothing else.\n",
    "- The data is the piped `.data`; write `mutate(...)` only (no `.data |>`).\n",
    "- The formula is filled down the whole column; translate it as a single\n",
    "  vectorised expression over the columns -- never reference row numbers.\n",
    "- Available columns in `.data`: ", cols_str, ".\n",
    "- Excel column letters map to these columns: ", map_str, ".\n",
    "- Use :: namespacing (e.g. dplyr::); never library() or require()."
  )
}

#' @keywords internal
#' @noRd
.hal_excel_user_prompt <- function(spec, data) {
  head_n <- min(5L, nrow(data))
  sample <- paste(utils::capture.output(print(head(data, head_n))), collapse = "\n")
  exp_str <- paste(format(head(spec$cached, head_n)), collapse = ", ")
  paste0(
    "Target column: ", spec$name, "\n",
    "Excel formula (filled down the column): ", spec$formula, "\n",
    "Input sample (head):\n", sample, "\n",
    "Expected output (head): ", exp_str
  )
}

#' @keywords internal
#' @noRd
.hal_excel_mismatch_report <- function(cmp) {
  if (!is.null(cmp$error)) {
    return(paste0("Your code ran but was wrong: ", cmp$error,
                  ". Return one mutate() that reproduces the Excel column."))
  }
  lines <- c(
    sprintf("Output did not match Excel (%d rows correct).", cmp$n_match),
    "First mismatches (sheet row / Excel value / your value):"
  )
  if (!is.null(cmp$mism)) {
    lines <- c(lines, sprintf("  row %s: Excel=%s, you=%s",
                              cmp$mism$row, cmp$mism$excel, cmp$mism$r))
  }
  paste(c(lines, "Fix the translation so every row matches Excel."),
        collapse = "\n")
}

# ------------------------------------------------------------------------------
# Result rows + display
# ------------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.hal_excel_row <- function(spec, code, status, n_match, n_total, note) {
  coverage <- if (is.null(n_match) || is.na(n_match)) sprintf("-/%d", n_total)
              else sprintf("%d/%d", n_match, n_total)
  data.frame(name = spec$name, formula = spec$formula, status = status,
             coverage = coverage, code = code %||% NA_character_,
             note = note %||% NA_character_, stringsAsFactors = FALSE)
}

#' Strip a leading `.data |>` / `.data %>%` the model may have added
#' @keywords internal
#' @noRd
.hal_excel_clean_code <- function(code) {
  code <- trimws(code)
  code <- sub("^\\.data\\s*\\|>\\s*", "", code)
  code <- sub("^\\.data\\s*%>%\\s*", "", code)
  trimws(code)
}

#' Assemble the generated script: read inputs + recreate the formula columns
#'
#' Verified columns chain into one `data |> mutate() |> ...` pipeline; anything
#' not verified is appended as commented stubs to review.
#' @keywords internal
#' @noRd
.hal_excel_assemble <- function(path, sheet, input_names, meta) {
  sel <- if (length(input_names)) {
    paste0("[c(", paste(sprintf('"%s"', input_names), collapse = ", "), ")]")
  } else ""
  read_line <- sprintf(
    'data <- openxlsx2::wb_to_df("%s", sheet = "%s", col_names = TRUE)%s',
    path, sheet, sel
  )

  verified <- meta[meta$status == "verified", , drop = FALSE]
  others   <- meta[meta$status != "verified", , drop = FALSE]

  lines <- c(
    sprintf('# Generated by hal_excel from "%s" (sheet: %s)', basename(path), sheet),
    "# Reads the input columns and recreates the formula columns in R.",
    "",
    read_line,
    ""
  )

  if (nrow(verified)) {
    lines <- c(lines, "result <- data |>")
    for (i in seq_len(nrow(verified))) {
      r <- verified[i, ]
      sep <- if (i < nrow(verified)) " |>" else ""
      lines <- c(lines, sprintf("  %s%s   # %s  [verified %s]",
                                r$code, sep, r$formula, r$coverage))
    }
  } else {
    lines <- c(lines, "# (no columns verified against Excel)")
  }

  if (nrow(others)) {
    lines <- c(lines, "", "# Not verified -- review before use:")
    for (i in seq_len(nrow(others))) {
      r <- others[i, ]
      code <- if (is.na(r$code)) "(no code)" else r$code
      lines <- c(lines, sprintf("# %s   # %s  [%s %s]",
                                code, r$formula, toupper(r$status), r$coverage))
    }
  }

  structure(paste(lines, collapse = "\n"), class = "hal_excel_code")
}

#' @param x A `hal_excel_code` object (the generated R script).
#' @param ... Ignored.
#' @rdname hal_excel
#' @export
print.hal_excel_code <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}

# ------------------------------------------------------------------------------
# Edit-in-place -- replace the hal_excel() call in the editor (like hal_do)
# ------------------------------------------------------------------------------

#' Replace the hal_excel() call in the active source editor with generated code
#'
#' Returns TRUE if it rewrote the buffer, FALSE otherwise (no rstudioapi, no
#' source editor, console, or the call couldn't be located) -- the caller then
#' falls back to printing. Gated by option `hal.edit_in_place` (default TRUE).
#' @keywords internal
#' @noRd
.hal_excel_edit_in_place <- function(code_text) {
  if (!isTRUE(getOption("hal.edit_in_place", TRUE))) return(FALSE)
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    return(FALSE)
  }
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(ctx) || !nzchar(ctx$path %||% "") || identical(ctx$id, "#console")) {
    return(FALSE)
  }
  range <- tryCatch(.hal_excel_find_call_range(ctx), error = function(e) NULL)
  if (is.null(range)) return(FALSE)
  tryCatch({
    rstudioapi::modifyRange(range, as.character(code_text), id = ctx$id)
    TRUE
  }, error = function(e) FALSE)
}

#' Range of the whole `hal_excel(...)` statement nearest the cursor
#'
#' From the start of the call's line through the matching close paren, so the
#' multi-line generated block replaces the entire statement (including any
#' `x <- ` prefix).
#' @keywords internal
#' @noRd
.hal_excel_find_call_range <- function(ctx) {
  contents <- ctx$contents
  n_lines <- length(contents)
  if (n_lines == 0L) return(NULL)

  cursor_row <- tryCatch(ctx$selection[[1L]]$range$start[[1L]],
                         error = function(e) 1L)
  search_start <- max(1L, cursor_row - 1L)

  target_row <- NULL
  call_col <- NULL
  for (row in seq(search_start, 1L)) {
    line <- contents[row]
    if (grepl("^\\s*#", line)) next
    pos <- regexpr("hal_excel\\(", line)
    if (pos > 0L) {
      before <- substr(line, 1L, pos - 1L)
      if (!grepl("#", before)) {
        target_row <- row
        call_col <- as.integer(pos)
        break
      }
    }
  }
  if (is.null(target_row)) return(NULL)

  paren_col <- call_col + nchar("hal_excel")
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
            # replace from the start of the statement's line ...
            start_pos <- rstudioapi::document_position(target_row, 1L)
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
