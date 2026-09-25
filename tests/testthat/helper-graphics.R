# Headless-safe recordable graphics device for plot-vision tests.
#
# pdf(NULL) can't record, but a file pdf device with
# dev.control(displaylist = "enable") supports recordPlot() without a
# screen -- the trick that makes plot capture testable offline/CI.

local_recordable_device <- function(env = parent.frame()) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  grDevices::dev.control(displaylist = "enable")
  dev <- grDevices::dev.cur()
  withr::defer({
    if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
    unlink(path)
  }, envir = env)
  dev
}
