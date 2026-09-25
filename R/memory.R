# ==============================================================================
# hal.md -- project memory file
# ==============================================================================
#
# A plain markdown file in the project root that persists context across
# sessions. The model reads it for background and can update it via its
# built-in edit tool. The user can edit it by hand anytime.
#
# hal offers to create it, but never writes it unasked: CRAN policy allows a
# package to write outside tempdir() only in an interactive session and only
# once the user confirms. See .hal_ensure_memory_file().
#
# The file is injected into the first turn of each hal() session as runtime
# context -- it never touches the base system prompt in R/prompts.R.

#' Default hal.md header template
#' @keywords internal
#' @noRd
.hal_memory_header <- function() {
  paste0(
    "# hal.md\n\n",
    "Project memory for hal. This file is read at the start of each session\n",
    "to provide context from prior work. Update it when you learn something\n",
    "worth preserving -- data descriptions, conclusions, conventions, etc.\n",
    "\n---\n\n"
  )
}

#' Offer to create hal.md in the working directory
#'
#' Asks before writing, because CRAN policy permits writing outside tempdir()
#' only in an interactive session and only after the user confirms. Never
#' writes in a non-interactive session. A decline is remembered for the rest
#' of the R session, per folder, so the user is not asked twice about the same
#' place -- and it survives hal_reset(), which only resets its own fields.
#' `options(hal.memory_prompt = FALSE)` turns the offer off entirely. An
#' existing hal.md is always read, whatever the answer.
#'
#' @return The memory file path, invisibly.
#' @keywords internal
#' @noRd
.hal_ensure_memory_file <- function() {
  path <- .hal_memory_path()
  if (file.exists(path)) return(invisible(path))
  if (!.hal_interactive()) return(invisible(path))
  if (!isTRUE(getOption("hal.memory_prompt", TRUE))) return(invisible(path))

  # Key the decline on the folder, which exists, not on the file, which does
  # not yet. On Linux and macOS normalizePath() can only resolve paths that
  # exist, so for a not-yet-created "hal.md" it returns "hal.md" unchanged in
  # every folder -- and one decline would silence the offer everywhere.
  # Windows resolves it regardless, which is why this only failed off Windows.
  session <- .hal_get_session()
  key <- file.path(normalizePath(dirname(path), mustWork = FALSE), basename(path))
  if (key %in% session$memory_declined) return(invisible(path))

  quiet <- isTRUE(getOption("hal.session_quiet", FALSE))
  file <- basename(path)
  msg <- paste0(
    "hal can keep project notes in ", file, " in this folder and read them ",
    "at the start of each session. Create ", file, "?"
  )

  if (!.hal_confirm(msg)) {
    session$memory_declined <- c(session$memory_declined, key)
    if (!quiet) {
      cli::cli_inform(c(
        "i" = "Not creating {.path {file}} -- hal still reads one if you add it yourself.",
        " " = "To stop this question: {.code options(hal.memory_prompt = FALSE)}"
      ))
    }
    return(invisible(path))
  }

  tryCatch({
    writeLines(.hal_memory_header(), path)
    if (!quiet) cli::cli_alert_info("Created {.path {path}} for project memory.")
  }, error = function(e) {
    # Non-fatal -- read-only directories, etc.
    NULL
  })
  invisible(path)
}

#' Read hal.md contents (excluding the header)
#'
#' Returns the content after the `---` separator. If the file doesn't exist
#' or has no content beyond the header, returns `""`.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
.hal_read_memory_file <- function() {
  path <- .hal_memory_path()
  if (!file.exists(path)) return("")

  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) return("")

  # Find the --- separator and return everything after it

  sep <- which(trimws(lines) == "---")
  if (length(sep) > 0) {
    after <- sep[length(sep)] + 1L
    if (after > length(lines)) return("")
    lines <- lines[after:length(lines)]
  }

  content <- trimws(paste(lines, collapse = "\n"))
  content
}

#' Resolve the hal.md path
#' @keywords internal
#' @noRd
.hal_memory_path <- function() {
  getOption("hal.memory_file", "hal.md")
}

#' Mockable interactive() wrapper (testthat can't bind over base)
#' @keywords internal
#' @noRd
.hal_interactive <- function() interactive()

#' Ask a yes/no question; isolated so tests can mock it
#' @return TRUE only on an explicit yes -- no, cancel, or no answer is FALSE.
#' @keywords internal
#' @noRd
.hal_confirm <- function(msg) isTRUE(utils::askYesNo(msg, default = FALSE))
