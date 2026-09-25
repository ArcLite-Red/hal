# Tests for hal.md project memory

# ---- path helper ------------------------------------------------------------

test_that(".hal_memory_path defaults to hal.md", {
  withr::with_options(list(hal.memory_file = NULL), {
    expect_equal(.hal_memory_path(), "hal.md")
  })
})

test_that(".hal_memory_path honors option", {
  withr::with_options(list(hal.memory_file = "custom.md"), {
    expect_equal(.hal_memory_path(), "custom.md")
  })
})

# ---- ensure file ------------------------------------------------------------

# CRAN policy: a package may write outside tempdir() only in an interactive
# session, and only after the user confirms. These tests pin that contract.

# Fresh temp working dir, and a clean slate for per-folder declines (the
# session env is package-level, so it outlives any one test).
local_memory_dir <- function(env = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(tmp, .local_envir = env)
  session <- .hal_get_session()
  old <- session$memory_declined
  session$memory_declined <- NULL
  withr::defer(session$memory_declined <- old, envir = env)
  tmp
}

test_that("creates hal.md with a header when the user confirms", {
  local_memory_dir()
  local_mocked_bindings(
    .hal_interactive = function() TRUE,
    .hal_confirm = function(msg) TRUE
  )
  withr::with_options(list(hal.memory_file = "hal.md", hal.session_quiet = TRUE), {
    .hal_ensure_memory_file()
    expect_true(file.exists("hal.md"))
    content <- readLines("hal.md")
    expect_true(any(grepl("# hal.md", content)))
    expect_true(any(grepl("---", content)))
  })
})

test_that("does not create hal.md when the user declines", {
  local_memory_dir()
  local_mocked_bindings(
    .hal_interactive = function() TRUE,
    .hal_confirm = function(msg) FALSE
  )
  withr::with_options(list(hal.memory_file = "hal.md", hal.session_quiet = TRUE), {
    .hal_ensure_memory_file()
    expect_false(file.exists("hal.md"))
  })
})

test_that("asks at most once per folder after a decline", {
  local_memory_dir()
  asked <- 0L
  local_mocked_bindings(
    .hal_interactive = function() TRUE,
    .hal_confirm = function(msg) { asked <<- asked + 1L; FALSE }
  )
  withr::with_options(list(hal.memory_file = "hal.md", hal.session_quiet = TRUE), {
    .hal_ensure_memory_file()
    .hal_ensure_memory_file()
    .hal_ensure_memory_file()
  })
  expect_identical(asked, 1L)
  expect_false(file.exists("hal.md"))
})

test_that("a decline in one folder does not suppress the offer in another", {
  local_memory_dir()
  asked <- 0L
  local_mocked_bindings(
    .hal_interactive = function() TRUE,
    .hal_confirm = function(msg) { asked <<- asked + 1L; FALSE }
  )
  withr::with_options(list(hal.memory_file = "hal.md", hal.session_quiet = TRUE), {
    .hal_ensure_memory_file()
    withr::with_dir(withr::local_tempdir(), .hal_ensure_memory_file())
  })
  expect_identical(asked, 2L)
})

test_that("never asks or writes in a non-interactive session", {
  local_memory_dir()
  local_mocked_bindings(
    .hal_interactive = function() FALSE,
    .hal_confirm = function(msg) stop("must not ask non-interactively")
  )
  withr::with_options(list(hal.memory_file = "hal.md", hal.session_quiet = TRUE), {
    expect_no_error(.hal_ensure_memory_file())
    expect_false(file.exists("hal.md"))
  })
})

test_that("hal.memory_prompt = FALSE never asks and never writes", {
  local_memory_dir()
  local_mocked_bindings(
    .hal_interactive = function() TRUE,
    .hal_confirm = function(msg) stop("must not ask when the prompt is off")
  )
  withr::with_options(
    list(hal.memory_file = "hal.md", hal.session_quiet = TRUE,
         hal.memory_prompt = FALSE),
    {
      expect_no_error(.hal_ensure_memory_file())
      expect_false(file.exists("hal.md"))
    }
  )
})

test_that("an existing hal.md is left alone and no question is asked", {
  local_memory_dir()
  writeLines("user content", "hal.md")
  local_mocked_bindings(
    .hal_interactive = function() TRUE,
    .hal_confirm = function(msg) stop("must not ask when the file exists")
  )
  withr::with_options(list(hal.memory_file = "hal.md", hal.session_quiet = TRUE), {
    expect_no_error(.hal_ensure_memory_file())
    expect_equal(readLines("hal.md"), "user content")
  })
})

test_that("a decline explains how to stop the question", {
  local_memory_dir()
  local_mocked_bindings(
    .hal_interactive = function() TRUE,
    .hal_confirm = function(msg) FALSE
  )
  withr::with_options(list(hal.memory_file = "hal.md", hal.session_quiet = FALSE), {
    expect_message(.hal_ensure_memory_file(), "hal.memory_prompt")
  })
})

# ---- read memory ------------------------------------------------------------

test_that(".hal_read_memory_file returns empty when file missing", {
  tmp <- withr::local_tempdir()
  old_wd <- setwd(tmp); on.exit(setwd(old_wd), add = TRUE)

  withr::with_options(list(hal.memory_file = "hal.md"), {
    expect_equal(.hal_read_memory_file(), "")
  })
})

test_that(".hal_read_memory_file returns empty for header-only file", {
  tmp <- withr::local_tempdir()
  old_wd <- setwd(tmp); on.exit(setwd(old_wd), add = TRUE)

  # Write the header directly. This used to call .hal_ensure_memory_file(),
  # which never creates the file under test (non-interactive) -- so the test
  # passed because the file was *missing*, not because it was header-only.
  writeLines(.hal_memory_header(), "hal.md")
  expect_true(file.exists("hal.md"))

  withr::with_options(list(hal.memory_file = "hal.md", hal.session_quiet = TRUE), {
    expect_equal(.hal_read_memory_file(), "")
  })
})

test_that(".hal_read_memory_file returns content after separator", {
  tmp <- withr::local_tempdir()
  old_wd <- setwd(tmp); on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("# hal.md", "", "---", "",
               "- sales is quarterly revenue data",
               "- Q4 2025 drop was structural"),
             "hal.md")

  withr::with_options(list(hal.memory_file = "hal.md"), {
    content <- .hal_read_memory_file()
    expect_match(content, "sales is quarterly revenue")
    expect_match(content, "Q4 2025")
  })
})

test_that(".hal_read_memory_file works with no separator", {
  tmp <- withr::local_tempdir()
  old_wd <- setwd(tmp); on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("just some facts", "about the project"), "hal.md")

  withr::with_options(list(hal.memory_file = "hal.md"), {
    content <- .hal_read_memory_file()
    expect_match(content, "just some facts")
  })
})
