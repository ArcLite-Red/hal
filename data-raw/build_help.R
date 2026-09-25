# ──────────────────────────────────────────────────────────────────────────────
# Build inst/help_text.rds — rendered Rd content for the hal_help MCP tool.
#
# Reads the curated public-API subset of man/*.Rd, renders each via
# tools::Rd2txt(), and writes a named list (topic -> character vector) to
# inst/help_text.rds. The MCP tool reads this bundle at session start and
# captures it in a closure so help lookups have no IPC round-trip and no
# dependency on tools::Rd_db at runtime.
#
# Run manually whenever an exported function's roxygen changes, or after adding
# a new export (it's picked up automatically):
#   Rscript data-raw/build_help.R
#
# Topics are auto-derived from NAMESPACE export() entries that have a man page,
# minus `omit` (R6 class generators and their low-level wrappers, which aren't
# the functional API the model should surface). S3 methods are never exported
# via export() so they don't appear. Keep `omit` in sync with the allowlist in
# tests/testthat/test-help-bundle.R.
# ──────────────────────────────────────────────────────────────────────────────

# R6 generators + low-level wrappers: exported for `::` access, but the
# functional API is what users (and the model) should see.
omit <- c("HalClient", "HalChat", "hal_client", "hal_chat")

ns <- readLines("NAMESPACE")
exports <- sub("^export\\(([^)]+)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
topics <- setdiff(exports, omit)
topics <- topics[file.exists(file.path("man", paste0(topics, ".Rd")))]
topics <- sort(topics)

man_dir <- "man"
if (!dir.exists(man_dir)) {
  stop("Run from package root. man/ not found at: ", normalizePath("."))
}

render_one <- function(topic) {
  rd_path <- file.path(man_dir, paste0(topic, ".Rd"))
  if (!file.exists(rd_path)) {
    stop("Missing Rd file for topic: ", topic, " (expected ", rd_path, ")")
  }
  rd <- tools::parse_Rd(rd_path)
  con <- textConnection(NULL, "w")
  on.exit(close(con), add = TRUE)
  tools::Rd2txt(rd, out = con, package = "hal")
  out <- textConnectionValue(con)
  if (!length(out) || all(!nzchar(out))) {
    stop("Rendered empty for topic: ", topic)
  }
  out
}

bundle <- stats::setNames(lapply(topics, render_one), topics)

inst_dir <- "inst"
if (!dir.exists(inst_dir)) dir.create(inst_dir, recursive = TRUE)
out_path <- file.path(inst_dir, "help_text.rds")
saveRDS(bundle, out_path, version = 2L)

message("Wrote ", out_path, " (", length(bundle), " topics, ",
        format(file.info(out_path)$size, big.mark = ","), " bytes)")
