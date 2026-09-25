# Shared helpers for the hal-bridge tests (test-bridge.R, test-client-vscode.R).
#
# .hal_bridge_port_file() resolves to a per-user app-data path that differs by
# platform (Windows: LOCALAPPDATA; POSIX: XDG_RUNTIME_DIR else HOME/.cache).
# These helpers redirect it into a scratch dir regardless of platform.

# Env vars that point .hal_bridge_port_file() at `dir` on every platform.
.bridge_env <- function(dir) {
  c(LOCALAPPDATA = dir, XDG_RUNTIME_DIR = dir, HOME = dir)
}

# Write a port file where the function will look, creating the hal-bridge/
# subdir. Must be called inside the matching withr::with_envvar(.bridge_env()).
# `contents` may be a list (written as JSON) or a character vector (written
# verbatim, for malformed-file tests). Returns the path written.
.write_port_file <- function(contents) {
  pf <- hal:::.hal_bridge_port_file()
  dir.create(dirname(pf), recursive = TRUE, showWarnings = FALSE)
  if (is.character(contents)) {
    writeLines(contents, pf)
  } else {
    jsonlite::write_json(contents, pf, auto_unbox = TRUE)
  }
  pf
}
