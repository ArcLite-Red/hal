test_that("NDJSON lines parse correctly", {
  # Simulate two NDJSON lines
  buffer <- '{"jsonrpc":"2.0","id":1,"result":{"text":"hello"}}\n{"jsonrpc":"2.0","id":2,"result":{"text":"world"}}\n'
  lines <- strsplit(buffer, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]

  expect_length(lines, 2)

  parsed1 <- jsonlite::fromJSON(lines[1], simplifyVector = FALSE)
  expect_equal(parsed1$id, 1)
  expect_equal(parsed1$result$text, "hello")

  parsed2 <- jsonlite::fromJSON(lines[2], simplifyVector = FALSE)
  expect_equal(parsed2$id, 2)
  expect_equal(parsed2$result$text, "world")
})

test_that("incomplete NDJSON line is detected", {
  buffer <- '{"jsonrpc":"2.0","id":1,"result":{"text":"hello"}}\n{"partial":'
  lines <- strsplit(buffer, "\n", fixed = TRUE)[[1]]

  # Last line has no trailing \n, so it's incomplete
  expect_false(endsWith(buffer, "\n"))
  expect_length(lines, 2)

  # First line parses fine
  parsed1 <- jsonlite::fromJSON(lines[1], simplifyVector = FALSE)
  expect_equal(parsed1$id, 1)

  # Second line fails to parse
  expect_error(jsonlite::fromJSON(lines[2], simplifyVector = FALSE))
})
