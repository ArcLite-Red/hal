# ==============================================================================
# hal Plot Vision -- capture plots drawn by eval_r for the model to see
# ==============================================================================
#
# Snapshot the graphics device state before eval, diff after, and render
# any new plot into a PNG the backend can attach as an image. Every
# graphics call is tryCatch'd: any failure degrades silently to the
# text-only behavior that existed before this feature.
#
# Backend gating: vscode (hal-bridge >= 0.1.4 emits LanguageModelDataPart)
# and claude (MCP image content blocks). Copilot's CLI image forwarding is
# unverified, so it stays text-only regardless of the option.
#
# Deliberate v1 limitations (documented in ?hal_configure):
#   - error paths return plain error text (no capture on failure)
#   - plots written to user-opened file devices (png(), pdf()) not echoed
#   - one image per eval: the current page at eval end (last plot wins;
#     par(mfrow=...) panels are one page and captured together)

#' Is plot vision active for the current backend?
#' @keywords internal
#' @noRd
.hal_plot_vision_enabled <- function() {
  isTRUE(getOption("hal.plot_vision", TRUE)) &&
    !identical(tryCatch(.hal_backend(), error = function(e) "copilot"),
               "copilot")
}

#' Snapshot device state before eval
#'
#' recordPlot() errors on an empty device -- NULL `rec` then means
#' "anything recordable afterward counts as a new plot".
#'
#' @return list(dev = device id, rec = recordedplot or NULL)
#' @keywords internal
#' @noRd
.hal_plot_snapshot <- function() {
  dev <- grDevices::dev.cur()
  rec <- if (dev != 1L) {
    tryCatch(grDevices::recordPlot(), error = function(e) NULL)
  } else {
    NULL
  }
  list(dev = dev, rec = rec)
}

#' Print a returned plot object so it actually draws
#'
#' eval() of parsed expressions never auto-prints, so a bare
#' `ggplot(...) + geom_point()` as the last expression returns the object
#' without drawing. Printing it here (a) draws it on the user's device --
#' it shows up in the Positron plots pane like any other plot -- and
#' (b) surfaces render errors (bad aes, missing column) that would
#' otherwise stay invisible until the user's pane tried to render.
#'
#' Reuses the governance timeout wrapper, so worst-case eval_r time is
#' ~2x hal.eval_timeout (eval + render).
#'
#' @param val The value returned by the user's code.
#' @return list(rendered = logical, error = character or NULL)
#' @keywords internal
#' @noRd
.hal_plot_render_returned <- function(val) {
  if (!inherits(val, c("ggplot", "trellis"))) {
    return(list(rendered = FALSE, error = NULL))
  }
  env <- new.env(parent = baseenv())
  env$val <- val
  err <- tryCatch(
    {
      .hal_eval_with_timeout(quote(print(val)), env)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  list(rendered = is.null(err), error = err)
}

#' Render a drawing function into a base64 PNG
#'
#' Opens our own png device, draws, closes, restores the user's device.
#' The on.exit is load-bearing: a leaked png device silently swallows all
#' subsequent user plots, and a lost active device strands them on the
#' null device.
#'
#' @param draw_fn Zero-arg function that draws (replayPlot or print).
#' @return list(mimeType, data) or NULL on any failure / >5MB.
#' @keywords internal
#' @noRd
.hal_render_png <- function(draw_fn) {
  if (!capabilities("png")) return(NULL)

  prev <- grDevices::dev.cur()
  path <- tempfile(fileext = ".png")
  opened <- tryCatch(
    {
      grDevices::png(path, width = 1200, height = 800, res = 120)
      grDevices::dev.cur()
    },
    error = function(e) NULL
  )
  if (is.null(opened)) return(NULL)

  on.exit({
    # Close our device if it's still open, then restore the user's.
    if (opened %in% grDevices::dev.list()) {
      tryCatch(grDevices::dev.off(opened), error = function(e) NULL)
    }
    if (prev > 1L && prev %in% grDevices::dev.list()) {
      tryCatch(grDevices::dev.set(prev), error = function(e) NULL)
    }
  }, add = TRUE)

  ok <- tryCatch({ draw_fn(); TRUE }, error = function(e) FALSE)
  # Close before reading so the file is flushed (on.exit still guards
  # the error paths above).
  if (opened %in% grDevices::dev.list()) {
    tryCatch(grDevices::dev.off(opened), error = function(e) NULL)
  }
  if (!ok || !file.exists(path)) return(NULL)

  size <- file.size(path)
  if (is.na(size) || size == 0 || size > 5e6) return(NULL)

  bytes <- readBin(path, "raw", n = size)
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(bytes))
  list(mimeType = "image/png", data = b64)
}

#' Detect and capture a plot drawn during eval
#'
#' @param snapshot From `.hal_plot_snapshot()` (taken before eval).
#' @param val The value the user's code returned.
#' @return list(image = list(mimeType, data) or NULL,
#'              render_error = character or NULL)
#' @keywords internal
#' @noRd
.hal_plot_capture <- function(snapshot, val) {
  none <- list(image = NULL, render_error = NULL)
  if (is.null(snapshot)) return(none)

  # Draw returned ggplot/trellis objects (also surfaces render errors)
  render <- .hal_plot_render_returned(val)
  if (!is.null(render$error)) {
    return(list(image = NULL, render_error = render$error))
  }

  dev <- grDevices::dev.cur()
  rec <- if (dev != 1L) {
    tryCatch(grDevices::recordPlot(), error = function(e) NULL)
  } else {
    NULL
  }

  if (is.null(rec)) {
    # No recordable display list (headless pdf device, bitmap device the
    # user opened, or a device that can't record). If we rendered a plot
    # object ourselves, we can still re-print it straight into a PNG.
    if (isTRUE(render$rendered)) {
      img <- .hal_render_png(function() print(val))
      return(list(image = img, render_error = NULL))
    }
    return(none)
  }

  changed <- dev != snapshot$dev ||
    is.null(snapshot$rec) ||
    !identical(rec, snapshot$rec)
  if (!changed) return(none)

  img <- .hal_render_png(function() grDevices::replayPlot(rec))
  if (is.null(img) && isTRUE(render$rendered)) {
    # replay failed but we can re-print the returned object directly
    img <- .hal_render_png(function() print(val))
  }
  list(image = img, render_error = NULL)
}
