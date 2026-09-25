# Plot vision -- graphics capture helpers
# (local_recordable_device fixture lives in helper-graphics.R)

test_that("enabled gate respects option and copilot backend", {
  withr::with_options(list(hal.backend = "claude", hal.plot_vision = TRUE), {
    expect_true(.hal_plot_vision_enabled())
  })
  withr::with_options(list(hal.backend = "claude", hal.plot_vision = FALSE), {
    expect_false(.hal_plot_vision_enabled())
  })
  withr::with_options(list(hal.backend = "copilot", hal.plot_vision = TRUE), {
    expect_false(.hal_plot_vision_enabled())
  })
})

test_that("snapshot with no device open has NULL recording", {
  skip_if(grDevices::dev.cur() != 1L, "a device is already open")
  snap <- .hal_plot_snapshot()
  expect_identical(unname(snap$dev), 1L)
  expect_null(snap$rec)
})

test_that("no new drawing yields no capture", {
  local_recordable_device()
  plot(1:5)
  snap <- .hal_plot_snapshot()
  cap <- .hal_plot_capture(snap, val = NULL)
  expect_null(cap$image)
  expect_null(cap$render_error)
})

test_that("a new base plot is captured as PNG base64", {
  skip_if_not(capabilities("png"))
  local_recordable_device()
  snap <- .hal_plot_snapshot()
  plot(1:10)
  cap <- .hal_plot_capture(snap, val = NULL)
  expect_type(cap$image, "list")
  expect_identical(cap$image$mimeType, "image/png")
  expect_false(grepl("[\r\n]", cap$image$data))
  # base64 decodes to PNG magic bytes
  bytes <- jsonlite::base64_dec(cap$image$data)
  expect_identical(as.integer(bytes[1:4]), c(137L, 80L, 78L, 71L))
})

test_that("incremental additions to an existing plot are captured", {
  skip_if_not(capabilities("png"))
  local_recordable_device()
  plot(1:10)
  snap <- .hal_plot_snapshot()
  abline(h = 5)
  cap <- .hal_plot_capture(snap, val = NULL)
  expect_type(cap$image, "list")
})

test_that("device restored after render", {
  skip_if_not(capabilities("png"))
  dev <- local_recordable_device()
  snap <- .hal_plot_snapshot()
  plot(1:10)
  .hal_plot_capture(snap, val = NULL)
  expect_identical(grDevices::dev.cur(), dev)
})

test_that("device restored even when draw_fn errors", {
  skip_if_not(capabilities("png"))
  dev <- local_recordable_device()
  n_before <- length(grDevices::dev.list())
  out <- .hal_render_png(function() stop("boom"))
  expect_null(out)
  expect_identical(grDevices::dev.cur(), dev)
  expect_identical(length(grDevices::dev.list()), n_before)
})

test_that("returned ggplot object is rendered and captured", {
  skip_if_not_installed("ggplot2")
  skip_if_not(capabilities("png"))
  local_recordable_device()
  snap <- .hal_plot_snapshot()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  cap <- .hal_plot_capture(snap, val = p)
  expect_type(cap$image, "list")
  expect_null(cap$render_error)
})

test_that("ggplot render error is surfaced, not thrown", {
  skip_if_not_installed("ggplot2")
  local_recordable_device()
  snap <- .hal_plot_snapshot()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(no_such_col, mpg)) +
    ggplot2::geom_point()
  cap <- expect_no_error(.hal_plot_capture(snap, val = p))
  expect_null(cap$image)
  expect_type(cap$render_error, "character")
})

test_that("NULL snapshot short-circuits", {
  cap <- .hal_plot_capture(NULL, val = NULL)
  expect_null(cap$image)
  expect_null(cap$render_error)
})
