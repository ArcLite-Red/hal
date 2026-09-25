# Tests for system prompt construction

# ---- default system prompt ---------------------------------------------------

test_that("default system prompt includes eval_r and R conventions", {
  prompt <- .hal_default_system_prompt()
  expect_match(prompt, "eval_r")
  expect_match(prompt, "R conventions", ignore.case = TRUE)
  expect_match(prompt, "Tidyverse")
})

test_that("default system prompt has no mode context for agent", {

  prompt <- .hal_default_system_prompt(mode = "agent")
  expect_false(grepl("Mode:", prompt))
})

test_that("plan mode appends plan context", {
  prompt <- .hal_default_system_prompt(mode = "plan")
  expect_match(prompt, "Mode: Plan")
  expect_match(prompt, "numbered multi-step plan", ignore.case = TRUE)
})

test_that("autopilot mode appends autopilot context", {
  prompt <- .hal_default_system_prompt(mode = "autopilot")
  expect_match(prompt, "Mode: Autopilot")
  expect_match(prompt, "autonomously")
})

test_that("NULL mode behaves like agent (no mode context)", {
  prompt <- .hal_default_system_prompt(mode = NULL)
  expect_false(grepl("Mode:", prompt))
})

# ---- pipe verb prompts -------------------------------------------------------

test_that("hal_do pipe prompt mentions .data and pipe style", {
  prompt <- .hal_do_pipe_system_prompt()
  expect_match(prompt, "\\.data")
  expect_match(prompt, "pipe")
})

test_that("hal_do standalone prompt mentions code-only output", {
  prompt <- .hal_do_standalone_system_prompt()
  expect_match(prompt, "ONLY valid R code")
})

# ---- retry prompt ------------------------------------------------------------

test_that("retry prompt includes error message and code", {
  rp <- .hal_do_retry_prompt("object not found", "mean(x)")
  expect_match(rp, "object not found")
  expect_match(rp, "mean\\(x\\)")
  expect_match(rp, "fixed code")
})

test_that("retry prompt works with empty code", {
  rp <- .hal_do_retry_prompt("some error", "")
  expect_match(rp, "some error")
  expect_false(grepl("Your Code", rp))
})
