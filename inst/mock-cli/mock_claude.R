# Mock Claude Code CLI for offline testing
#
# Emits NDJSON on stdout in the same stream-json format as `claude -p
# --output-format stream-json --include-partial-messages`. One invocation =
# one prompt turn (matches the real CLI; Claude Code spawns a fresh
# subprocess per turn and tracks state via --session-id / --resume).
#
# Scenario name is passed as the first positional arg (via helper's
# prefix_args), followed by the real claude-style flags the client produces:
#   -p <text> --output-format stream-json --include-partial-messages
#   --verbose --model <id> --permission-mode <mode>
#   [--mcp-config <path> --strict-mcp-config --allowedTools <list>]
#   [--session-id <uuid> | --resume <uuid>]
#
# Multi-turn state is keyed by session uuid and persisted to a tempfile so
# successive subprocesses can observe their own history.
#
# No hal dependency — only jsonlite.

suppressPackageStartupMessages(library(jsonlite))

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

raw_args <- commandArgs(trailingOnly = TRUE)
scenario <- if (length(raw_args) >= 1L) raw_args[1L] else "basic"
cli_args <- if (length(raw_args) >= 2L) raw_args[-1L] else character()

# -- Arg parsing --------------------------------------------------------------

get_flag <- function(args, name) {
  i <- match(name, args)
  if (is.na(i) || i == length(args)) return(NULL)
  args[i + 1L]
}

prompt_text   <- get_flag(cli_args, "-p") %||% ""
model_id      <- get_flag(cli_args, "--model") %||% "mock-claude-haiku"
session_id    <- get_flag(cli_args, "--session-id")
resume_id     <- get_flag(cli_args, "--resume")
permission_mode <- get_flag(cli_args, "--permission-mode") %||% "default"
is_resume     <- !is.null(resume_id)
sid           <- session_id %||% resume_id %||% "mock-session"

# -- NDJSON output ------------------------------------------------------------

send <- function(obj) {
  msg <- toJSON(obj, auto_unbox = TRUE, null = "null")
  cat(msg, "\n", sep = "", file = stdout())
  flush(stdout())
}

text_delta <- function(text) {
  send(list(
    type = "stream_event",
    event = list(
      type = "content_block_delta",
      delta = list(type = "text_delta", text = text)
    )
  ))
}

thinking_delta <- function(text) {
  send(list(
    type = "stream_event",
    event = list(
      type = "content_block_delta",
      delta = list(type = "thinking_delta", thinking = text)
    )
  ))
}

tool_use <- function(id, name, input) {
  send(list(
    type = "assistant",
    message = list(
      content = list(
        list(type = "tool_use", id = id, name = name, input = input)
      )
    )
  ))
}

tool_result <- function(id, content, is_error = FALSE) {
  send(list(
    type = "user",
    message = list(
      content = list(
        list(
          type = "tool_result",
          tool_use_id = id,
          content = content,
          is_error = is_error
        )
      )
    )
  ))
}

rate_limit <- function(status = "allowed",
                      resets_at = as.integer(Sys.time()) + 3600L,
                      overage_status = "allowed",
                      overage_disabled_reason = NULL) {
  info <- list(
    rateLimitType = "five_hour",
    status = status,
    resetsAt = resets_at,
    overageStatus = overage_status
  )
  if (!is.null(overage_disabled_reason)) {
    info$overageDisabledReason <- overage_disabled_reason
  }
  send(list(type = "rate_limit_event", rate_limit_info = info))
}

finish <- function(reason = "end_turn") {
  send(list(type = "result", stop_reason = reason, session_id = sid))
}

# The real CLI opens every stream with a system/init frame carrying the
# session id -- that frame is how the client learns the session now exists in
# Claude Code's store (and so must be --resume'd, not re-created, on the next
# turn even if this turn fails).
system_init <- function() {
  send(list(
    type = "system",
    subtype = "init",
    session_id = sid,
    model = model_id
  ))
}

# -- Multi-turn state ---------------------------------------------------------
# Persist a small counter keyed by session uuid so scenario=multi_turn can
# observe its own history across subprocess invocations.
#
# IMPORTANT: tempdir() is session-scoped and is FRESH for every Rscript
# invocation — state written there vanishes the moment this subprocess
# exits. dirname(tempdir()) gives the OS-level temp root (e.g. /tmp on
# Unix, %TEMP% on Windows), which is shared across processes.

state_dir <- function() {
  # Honour an explicit override from the test harness; otherwise fall back to
  # a stable subdir under the OS temp root so successive Rscript invocations
  # can see each other's state (tempdir() is session-scoped and disappears
  # with the subprocess). The harness is responsible for cleanup — scoping
  # everything under a single directory makes that one unlink() call.
  d <- Sys.getenv("HAL_MOCK_STATE_DIR", "")
  if (!nzchar(d)) d <- file.path(dirname(tempdir()), "hal_mock_claude")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

state_path <- function(sid) {
  file.path(state_dir(), paste0(sid, ".rds"))
}

read_state <- function(sid) {
  p <- state_path(sid)
  if (!file.exists(p)) return(list(turns = 0L))
  tryCatch(readRDS(p), error = function(e) list(turns = 0L))
}

write_state <- function(sid, st) {
  saveRDS(st, state_path(sid))
}

# -- Scenarios ----------------------------------------------------------------

scenario_basic <- function() {
  text_delta("Hello from ")
  text_delta("MockClaude.")
  finish()
}

scenario_echo <- function() {
  text_delta(prompt_text)
  finish()
}

scenario_thinking <- function() {
  thinking_delta("Let me think...")
  thinking_delta(" Okay.")
  text_delta("Done thinking.")
  finish()
}

scenario_tool_use <- function() {
  # Emit a tool_use block but NO corresponding tool_result — sufficient to
  # test that the client records the tool_call on the turn.
  text_delta("Calling a tool. ")
  tool_use("tool_001", "view_file", list(path = "README.md"))
  text_delta("Tool invoked.")
  finish()
}

scenario_tool_roundtrip <- function() {
  # tool_use followed by a matching tool_result — validates that the client
  # threads status/result back to the originating tool_call entry.
  tool_use("tool_rt_ok",  "view_file", list(path = "OK.md"))
  tool_result("tool_rt_ok", "file contents here", is_error = FALSE)
  tool_use("tool_rt_err", "view_file", list(path = "MISSING.md"))
  tool_result("tool_rt_err", "file not found", is_error = TRUE)
  text_delta("Done.")
  finish()
}

scenario_rate_limit <- function() {
  rate_limit(
    status = "allowed",
    resets_at = 1800000000L,  # fixed deterministic value for tests
    overage_status = "allowed"
  )
  text_delta("OK.")
  finish()
}

scenario_multi_turn <- function() {
  st <- read_state(sid)
  st$turns <- st$turns + 1L
  write_state(sid, st)
  text_delta(sprintf("Turn %d on session %s.", st$turns, substr(sid, 1, 8)))
  finish()
}

scenario_error <- function() {
  # Write to stderr and exit non-zero (the client treats this as a transport
  # error and calls cli_abort).
  cat("mock_claude: intentional error scenario\n", file = stderr())
  quit(save = "no", status = 2)
}

scenario_quiet_gap <- function() {
  # Emit one chunk, then go quiet for longer than a short prompt timeout.
  # Used to prove that time the client spends servicing IPC (running the
  # user's R code) is credited back to the deadline rather than counted
  # as the model stalling.
  text_delta("working...")
  Sys.sleep(as.numeric(Sys.getenv("HAL_MOCK_GAP_SECONDS", "4")))
  text_delta("done.")
  finish()
}

scenario_die_after_init <- function() {
  # Session gets created, then the CLI dies before emitting a result. The
  # client must still record the session as created so the next prompt
  # resumes it instead of re-sending a now-live --session-id.
  system_init()
  text_delta("partial...")
  quit(save = "no", status = 3)
}

scenario_slow <- function() {
  # Useful for cancel tests. Dribble text chunks with sleeps; client should
  # be able to interrupt mid-stream.
  for (i in 1:20) {
    text_delta(sprintf("chunk-%02d ", i))
    Sys.sleep(0.2)
  }
  finish()
}

# -- Dispatch -----------------------------------------------------------------

# The real CLI announces the session before anything else; mirror that for
# every scenario except the ones deliberately testing a pre-session failure.
if (!scenario %in% c("error", "die_after_init")) system_init()

switch(scenario,
  basic          = scenario_basic(),
  echo           = scenario_echo(),
  thinking       = scenario_thinking(),
  tool_use       = scenario_tool_use(),
  tool_roundtrip = scenario_tool_roundtrip(),
  rate_limit     = scenario_rate_limit(),
  multi_turn     = scenario_multi_turn(),
  error          = scenario_error(),
  die_after_init = scenario_die_after_init(),
  quiet_gap      = scenario_quiet_gap(),
  slow           = scenario_slow(),
  # default
  scenario_basic()
)
