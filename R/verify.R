# ==============================================================================
# hal Verification -- before/after transform reports for hal_do()
# ==============================================================================
#
# Report-only by design: hal_do()'s retry loop never sees these results.
# Deltas (dropped rows, new NAs) are often intentional -- "filter mpg > 20"
# legitimately shrinks the data -- so verification informs rather than
# gates. Suspicious patterns (output identical to input, 0-row output) get
# a classed warning; everything else is a compact info line plus a full
# report attached as attr(result, "hal_verify").

#' Compare a data frame before and after an LLM transform
#'
#' Pure structural comparison -- no reference to the prompt or intent.
#' Tolerates any data.frame subclass (tibble, data.table, grouped_df),
#' duplicate column names (first occurrence wins), and list-columns.
#'
#' @param before The input data frame (`.data`).
#' @param after The transformed result.
#' @return A `hal_verification` S3 object.
#' @keywords internal
#' @noRd
.hal_verify_transform <- function(before, after) {
  # Common columns by name; first occurrence on duplicates
  nb <- names(before)
  na_ <- names(after)
  common <- intersect(nb, na_)

  # NA counts per common column (works on list-columns too)
  count_na <- function(df, cols) {
    vapply(cols, function(nm) {
      col <- df[[nm]]
      if (is.null(col)) return(NA_integer_)
      sum(is.na(col))
    }, integer(1L))
  }
  na_before <- count_na(before, common)
  na_after <- count_na(after, common)

  # Class changes on common columns
  cls <- function(df, nm) paste(class(df[[nm]]), collapse = "/")
  class_changes <- Filter(Negate(is.null), stats::setNames(lapply(common, function(nm) {
    cb <- cls(before, nm)
    ca <- cls(after, nm)
    if (identical(cb, ca)) NULL else list(before = cb, after = ca)
  }), common))

  same_nrow <- nrow(before) == nrow(after)

  structure(
    list(
      nrow_before = nrow(before),
      nrow_after = nrow(after),
      ncol_before = ncol(before),
      ncol_after = ncol(after),
      cols_added = setdiff(na_, nb),
      cols_removed = setdiff(nb, na_),
      class_changes = class_changes,
      na_before = na_before,
      na_after = na_after,
      # Row-count robust: a column that had zero NAs and now has some is
      # the classic failed-coercion signal, regardless of filtering.
      new_na_cols = common[!is.na(na_before) & !is.na(na_after) &
                             na_before == 0L & na_after > 0L],
      # Only meaningful when row count is unchanged
      na_increase_cols = if (same_nrow) {
        common[!is.na(na_before) & !is.na(na_after) & na_after > na_before]
      } else {
        character()
      },
      identical_output = identical(before, after),
      zero_rows = nrow(after) == 0L
    ),
    class = "hal_verification"
  )
}

#' @export
format.hal_verification <- function(x, ...) {
  lines <- character()
  lines <- c(lines, sprintf("rows: %d -> %d", x$nrow_before, x$nrow_after))
  lines <- c(lines, sprintf("cols: %d -> %d", x$ncol_before, x$ncol_after))
  if (length(x$cols_added)) {
    lines <- c(lines, paste0("added: ", paste(x$cols_added, collapse = ", ")))
  }
  if (length(x$cols_removed)) {
    lines <- c(lines, paste0("removed: ", paste(x$cols_removed, collapse = ", ")))
  }
  if (length(x$class_changes)) {
    chg <- vapply(names(x$class_changes), function(nm) {
      cc <- x$class_changes[[nm]]
      paste0(nm, " (", cc$before, " -> ", cc$after, ")")
    }, character(1L))
    lines <- c(lines, paste0("class changed: ", paste(chg, collapse = ", ")))
  }
  if (length(x$new_na_cols)) {
    counts <- x$na_after[x$new_na_cols]
    lines <- c(lines, paste0(
      "new NAs: ",
      paste(sprintf("%s(%d)", x$new_na_cols, counts), collapse = ", ")
    ))
  }
  extra_inc <- setdiff(x$na_increase_cols, x$new_na_cols)
  if (length(extra_inc)) {
    lines <- c(lines, paste0(
      "NA count increased: ",
      paste(sprintf("%s(%d -> %d)", extra_inc,
                    x$na_before[extra_inc], x$na_after[extra_inc]),
            collapse = ", ")
    ))
  }
  if (isTRUE(x$identical_output)) {
    lines <- c(lines, "output is identical to input")
  }
  if (isTRUE(x$zero_rows)) {
    lines <- c(lines, "output has 0 rows")
  }
  lines
}

#' @export
print.hal_verification <- function(x, ...) {
  cat("<hal_verification>\n")
  cat(paste0("  ", format(x), collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' Apply verification to a hal_do pipe-mode result
#'
#' Gate + compute + attach + display + suspicious-pattern warnings.
#' Report-only: the caller's retry loop has already exited; nothing here
#' changes control flow. Verification itself must never fail the pipeline
#' (tryCatch), and the report is computed BEFORE the attribute is attached
#' -- attaching changes `result`, which would poison the identical_output
#' check.
#'
#' @param before The input data frame (`.data`).
#' @param result The transformed result (returned, possibly with
#'   `hal_verify` attribute attached).
#' @param verify NULL (use `hal.verify` option, default TRUE) or logical.
#' @return `result`, with `attr(result, "hal_verify")` set when verification
#'   ran.
#' @keywords internal
#' @noRd
.hal_do_verify <- function(before, result, verify = NULL) {
  if (!is.data.frame(before) || !is.data.frame(result)) return(result)
  if (!isTRUE(verify %||% getOption("hal.verify", TRUE))) return(result)

  v <- tryCatch(.hal_verify_transform(before, result),
                error = function(e) NULL)
  if (is.null(v)) return(result)

  attr(result, "hal_verify") <- v
  cli::cli_alert_info(.hal_verify_oneliner(v))
  if (isTRUE(v$identical_output)) {
    cli::cli_warn(
      "hal_do: output is identical to input -- the transform may not have applied.",
      class = c("hal_do_warning", "hal_warning")
    )
  } else if (isTRUE(v$zero_rows)) {
    cli::cli_warn(
      "hal_do: transform returned 0 rows.",
      class = c("hal_do_warning", "hal_warning")
    )
  }
  result
}

#' One-line summary for console display after hal_do()
#'
#' Omits segments with nothing to say; reports "no structural changes"
#' when the frames match shape-for-shape.
#'
#' @param v A `hal_verification` object.
#' @return Character scalar.
#' @keywords internal
#' @noRd
.hal_verify_oneliner <- function(v) {
  segs <- character()
  if (v$nrow_before != v$nrow_after) {
    segs <- c(segs, sprintf("%d -> %d rows", v$nrow_before, v$nrow_after))
  }
  if (length(v$cols_added)) {
    segs <- c(segs, sprintf("+%d col%s (%s)", length(v$cols_added),
                            if (length(v$cols_added) > 1L) "s" else "",
                            paste(v$cols_added, collapse = ", ")))
  }
  if (length(v$cols_removed)) {
    segs <- c(segs, sprintf("-%d col%s (%s)", length(v$cols_removed),
                            if (length(v$cols_removed) > 1L) "s" else "",
                            paste(v$cols_removed, collapse = ", ")))
  }
  if (length(v$class_changes)) {
    segs <- c(segs, paste0("class changed: ",
                           paste(names(v$class_changes), collapse = ", ")))
  }
  na_cols <- union(v$new_na_cols, v$na_increase_cols)
  if (length(na_cols)) {
    counts <- v$na_after[na_cols] - ifelse(is.na(v$na_before[na_cols]), 0L,
                                           v$na_before[na_cols])
    segs <- c(segs, paste0("new NAs: ",
                           paste(sprintf("%s(%d)", na_cols, counts),
                                 collapse = ", ")))
  }
  if (!length(segs)) {
    if (isTRUE(v$identical_output)) return("hal_do: output identical to input")
    return("hal_do: no structural changes")
  }
  paste0("hal_do: ", paste(segs, collapse = " | "))
}
