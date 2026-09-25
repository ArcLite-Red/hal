# test-history.R -- hal_history() tests

test_that("history returns empty when no session", {
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- NULL
  on.exit(session$chat <- old_chat, add = TRUE)

  expect_message(hal_history(), "No active session")
})

test_that("history returns empty data.frame format when no session", {
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- NULL
  on.exit(session$chat <- old_chat, add = TRUE)

  result <- hal_history(format = "data.frame")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("history returns empty text format when no session", {
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- NULL
  on.exit(session$chat <- old_chat, add = TRUE)

  result <- hal_history(format = "text")
  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("history with mock chat returns turns", {
  chat <- mock_chat("multi_turn")
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- chat
  on.exit(session$chat <- old_chat, add = TRUE)

  # Send a prompt to create turns
  chat$chat("hello")

  result <- hal_history(format = "data.frame")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)  # system + user + assistant at minimum
  expect_true("role" %in% names(result))
  expect_true("content" %in% names(result))

  chat$get_client()$stop()
})

test_that("history filters by role", {
  chat <- mock_chat("multi_turn")
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- chat
  on.exit(session$chat <- old_chat, add = TRUE)

  chat$chat("hello")

  result <- hal_history(role = "user", format = "data.frame")
  expect_true(all(result$role == "user"))

  result2 <- hal_history(role = "assistant", format = "data.frame")
  expect_true(all(result2$role == "assistant"))

  chat$get_client()$stop()
})

test_that("history filters by pattern", {
  chat <- mock_chat("multi_turn")
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- chat
  on.exit(session$chat <- old_chat, add = TRUE)

  chat$chat("hello")

  result <- hal_history(pattern = "Turn 1", format = "data.frame")
  expect_true(nrow(result) >= 1)
  expect_true(any(grepl("Turn 1", result$content)))

  chat$get_client()$stop()
})

test_that("history respects n parameter", {
  chat <- mock_chat("multi_turn")
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- chat
  on.exit(session$chat <- old_chat, add = TRUE)

  chat$chat("first")
  chat$chat("second")

  all_turns <- hal_history(format = "data.frame")
  limited <- hal_history(n = 2, format = "data.frame")
  expect_true(nrow(limited) <= 2)
  expect_true(nrow(all_turns) >= nrow(limited))

  chat$get_client()$stop()
})

test_that("history text format returns character vector", {
  chat <- mock_chat("multi_turn")
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- chat
  on.exit(session$chat <- old_chat, add = TRUE)

  chat$chat("hello")

  result <- hal_history(format = "text")
  expect_type(result, "character")
  expect_true(length(result) >= 2)
  expect_true(any(grepl("\\[user\\]", result)))
  expect_true(any(grepl("\\[assistant\\]", result)))

  chat$get_client()$stop()
})

test_that("history console format returns invisible NULL", {
  chat <- mock_chat("multi_turn")
  session <- .hal_get_session()
  old_chat <- session$chat
  session$chat <- chat
  on.exit(session$chat <- old_chat, add = TRUE)

  chat$chat("hello")

  result <- hal_history(format = "console")
  expect_null(result)

  chat$get_client()$stop()
})
