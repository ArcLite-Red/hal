#' Remove NULL entries from a list
#'
#' @param x A list.
#' @return The list with NULL elements removed.
#' @noRd
compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' Create an empty named list
#'
#' jsonlite serializes `list()` as `[]` (JSON array) but
#' `structure(list(), names = character())` as `{}` (JSON object).
#' Many ACP params require empty objects.
#'
#' @return An empty named list that serializes to `{}`.
#' @noRd
named_list <- function() {
  structure(list(), names = character())
}

# IPC primitives shared by the parent (client.R, client-claude.R) and the
# MCP subprocess. On Windows, EDR/AV briefly opens a file post-write to scan
# it; a concurrent open or rename during that window fails with "Permission
# denied". Both helpers retry on failure rather than falling back to a
# non-atomic write (which would let a reader observe a partial file).
.HAL_IPC_RETRIES <- 5
.HAL_IPC_BACKOFF <- 0.05  # seconds; total budget ~250ms

#' Atomically write text to `path` via tmp + rename, retrying on failure.
#'
#' @return TRUE on success, FALSE if all retries failed (file not written).
#' @noRd
.hal_atomic_write <- function(path, text,
                              attempts = .HAL_IPC_RETRIES,
                              backoff = .HAL_IPC_BACKOFF) {
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  ok_write <- tryCatch({
    writeLines(text, tmp)
    TRUE
  }, error = function(e) FALSE)
  if (!ok_write) return(FALSE)

  for (i in seq_len(attempts)) {
    ok <- suppressWarnings(file.rename(tmp, path))
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(backoff)
  }
  unlink(tmp)
  FALSE
}

#' Read + parse JSON from `path`, retrying on transient open errors.
#'
#' Returns the parsed object or `NULL` on persistent failure.
#' @noRd
.hal_safe_read_json <- function(path,
                                attempts = .HAL_IPC_RETRIES,
                                backoff = .HAL_IPC_BACKOFF) {
  for (i in seq_len(attempts)) {
    out <- tryCatch(
      jsonlite::fromJSON(
        readLines(path, warn = FALSE),
        simplifyVector = FALSE
      ),
      error = function(e) e,
      warning = function(w) w
    )
    if (!inherits(out, c("error", "warning"))) return(out)
    Sys.sleep(backoff)
  }
  NULL
}
