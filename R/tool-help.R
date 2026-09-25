# ──────────────────────────────────────────────────────────────────────────────
# hal_help -- built-in MCP tool that lets the model read hal's own man pages
# instead of guessing about hal's features.
#
# Content comes from inst/help_text.rds (built by data-raw/build_help.R from
# the curated public-API subset of man/*.Rd). The bundle is captured in the
# tool's `fun` closure so it serializes into the MCP subprocess via saveRDS
# alongside the function -- no IPC, no system.file lookup at call time.
# ──────────────────────────────────────────────────────────────────────────────

#' Tool definition for the hal_help MCP tool
#'
#' Returns a tool def list ready for `chat$register_tool()`. The bundle is
#' loaded once at definition time and lives inside the closure environment.
#'
#' @return A tool definition list, or `NULL` if the help bundle is missing
#'   (defensive — should be present in every installed/loaded build).
#' @keywords internal
#' @noRd
.hal_help_tool_def <- function() {
  bundle_path <- system.file("help_text.rds", package = "hal")
  if (!nzchar(bundle_path) || !file.exists(bundle_path)) {
    return(NULL)
  }
  help_bundle <- readRDS(bundle_path)
  topics <- names(help_bundle)

  fun <- local({
    bundle <- help_bundle
    valid <- topics
    function(topic) {
      if (missing(topic) || !is.character(topic) || length(topic) != 1L ||
          !nzchar(topic)) {
        return(paste0(
          "hal_help: 'topic' must be a single non-empty string. ",
          "Available topics: ", paste(valid, collapse = ", ")
        ))
      }
      if (!topic %in% valid) {
        return(paste0(
          "Unknown hal topic: '", topic, "'. ",
          "Available topics: ", paste(valid, collapse = ", ")
        ))
      }
      paste(bundle[[topic]], collapse = "\n")
    }
  })

  list(
    name = "hal_help",
    description = paste(
      "Return rendered R help text for a hal package topic. Use this when",
      "the user asks about hal's features, functions, or how to use them",
      "-- it returns the authoritative man-page content. Pass the function",
      "name as the topic (e.g. 'hal_ask'). The 'hal' topic is the package",
      "overview. Do not guess about hal's API; call this tool first."
    ),
    fun = fun,
    parameters = list(
      type = "object",
      properties = list(
        topic = list(
          type = "string",
          description = paste0(
            "Function name to look up. One of: ",
            paste(topics, collapse = ", ")
          )
        )
      ),
      required = list("topic")
    )
  )
}
