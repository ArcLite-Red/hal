test_that("turn() creates hal_turn objects", {
  t <- turn("user", "hello")
  expect_s3_class(t, "hal_turn")
  expect_equal(t$role, "user")
  expect_equal(t$content, "hello")
})

test_that("turn() strips NULL fields", {
  t <- turn("assistant")
  expect_false("content" %in% names(t))
  expect_false("tool_calls" %in% names(t))
  expect_false("thoughts" %in% names(t))
})

test_that("turn() works for all roles", {
  for (role in c("user", "assistant", "system")) {
    t <- turn(role, "text")
    expect_s3_class(t, "hal_turn")
    expect_equal(t$role, role)
    expect_equal(t$content, "text")
  }
})

test_that("turn() stores tool_calls and thoughts", {
  tc <- hal_tool_call(tool_call_id = "tc1", title = "Read file", kind = "read")
  t <- turn("assistant", "response", tool_calls = list(tc), thoughts = c("thinking..."))
  expect_length(t$tool_calls, 1)
  expect_s3_class(t$tool_calls[[1]], "hal_tool_call")
  expect_equal(t$thoughts, "thinking...")
})

test_that("hal_response() creates proper S3 objects", {
  tc <- hal_tool_call(tool_call_id = "tc1", title = "Read", kind = "read", status = "completed")
  resp <- hal_response(
    text = "hello",
    stop_reason = "end_turn",
    tool_calls = list(tc),
    thoughts = c("hmm", "let me think")
  )
  expect_s3_class(resp, "hal_response")
  expect_equal(resp$text, "hello")
  expect_equal(resp$stop_reason, "end_turn")
  expect_length(resp$tool_calls, 1)
  expect_equal(resp$thoughts, c("hmm", "let me think"))
})

test_that("hal_tool_call() creates proper S3 objects", {
  tc <- hal_tool_call(
    tool_call_id = "tc1",
    title = "Viewing DESCRIPTION",
    kind = "read",
    status = "completed",
    input = list(path = "/tmp/DESCRIPTION"),
    output = "Package: hal"
  )
  expect_s3_class(tc, "hal_tool_call")
  expect_equal(tc$tool_call_id, "tc1")
  expect_equal(tc$kind, "read")
  expect_equal(tc$status, "completed")
  expect_equal(tc$input$path, "/tmp/DESCRIPTION")
  expect_equal(tc$output, "Package: hal")
})

test_that("format.hal_tool_call() returns formatted string", {
  tc <- hal_tool_call(title = "Read file", kind = "read", status = "completed")
  formatted <- format(tc)
  expect_match(formatted, "read")
  expect_match(formatted, "Read file")
})

test_that("format.hal_tool_call() uses ASCII icons on Windows", {
  # Simulate Windows platform
  withr::with_envvar(c(COPILOT_USE_UNICODE = NA_character_), {
    # We can't change .Platform$OS.type, so test the logic directly:
    # on actual Windows this test verifies ASCII; on other platforms
    # we at least verify the Unicode path works.
    tc_ok <- hal_tool_call(title = "Done", kind = "read", status = "completed")
    tc_fail <- hal_tool_call(title = "Broke", kind = "write", status = "failed")
    tc_pend <- hal_tool_call(title = "Wait", kind = "tool", status = "pending")
    tc_other <- hal_tool_call(title = "Misc", kind = "tool", status = "unknown")

    fmt_ok <- format(tc_ok)
    fmt_fail <- format(tc_fail)
    fmt_pend <- format(tc_pend)
    fmt_other <- format(tc_other)

    if (.Platform$OS.type == "windows") {
      expect_match(fmt_ok, "\\[OK\\]", fixed = FALSE)
      expect_match(fmt_fail, "\\[FAIL\\]", fixed = FALSE)
      expect_match(fmt_pend, "\\[\\.\\.\\.\\]", fixed = FALSE)
      expect_match(fmt_other, "\\[\\*\\]", fixed = FALSE)
    } else {
      # Unicode path — check for known codepoints
      expect_match(fmt_ok, "\u2713")
      expect_match(fmt_fail, "\u2717")
      expect_match(fmt_pend, "\u2026")
      expect_match(fmt_other, "\u2022")
    }
  })
})

test_that("format.hal_tool_call() respects COPILOT_USE_UNICODE override", {
  withr::with_envvar(c(COPILOT_USE_UNICODE = "true"), {
    tc <- hal_tool_call(title = "Done", kind = "read", status = "completed")
    formatted <- format(tc)
    # With override, should always use Unicode regardless of platform
    expect_match(formatted, "\u2713")
  })
})

test_that("print methods don't error", {
  tc <- hal_tool_call(title = "Read file", kind = "read", status = "completed")
  resp <- hal_response(text = "hello", tool_calls = list(tc))
  t <- turn("assistant", "hello", tool_calls = list(tc))

  expect_output(print(tc))
  expect_output(print(resp))
  expect_output(print(t))
})
