# Tests that inst/help_text.rds stays in sync with man/ and the export list.
# If these fail, run: Rscript data-raw/build_help.R

render_topic <- function(pkg_root, topic) {
  rd <- tools::parse_Rd(file.path(pkg_root, "man", paste0(topic, ".Rd")))
  con <- textConnection(NULL, "w")
  on.exit(close(con), add = TRUE)
  tools::Rd2txt(rd, out = con, package = "hal")
  textConnectionValue(con)
}

test_that("inst/help_text.rds matches a fresh render of man/", {
  skip_on_cran()
  pkg_root <- testthat::test_path("..", "..")
  skip_if_not(dir.exists(file.path(pkg_root, "man")),
              "man/ not available (installed-package test run)")

  bundle_path <- file.path(pkg_root, "inst", "help_text.rds")
  expect_true(file.exists(bundle_path))

  current <- readRDS(bundle_path)
  fresh <- stats::setNames(
    lapply(names(current), function(t) render_topic(pkg_root, t)),
    names(current)
  )

  expect_identical(
    fresh, current,
    info = "Help bundle is stale. Run: Rscript data-raw/build_help.R"
  )
})

test_that("hal_help covers all exported hal_* functions", {
  bundle_path <- system.file("help_text.rds", package = "hal")
  skip_if(!nzchar(bundle_path) || !file.exists(bundle_path),
          "help bundle not installed")

  # All exports except S3 methods (dotted names dispatch-only, never topics).
  exported <- getNamespaceExports("hal")
  exported <- exported[!grepl("\\.", exported)]
  covered  <- names(readRDS(bundle_path))

  # Allowlist intentional omissions -- keep in sync with `omit` in
  # data-raw/build_help.R. R6 class generators + low-level wrappers are
  # exported for `::` access but the functional API is what users call.
  omit <- c("hal_client", "hal_chat", "HalClient", "HalChat")

  missing_topics <- setdiff(exported, c(covered, omit))
  expect_identical(
    missing_topics, character(),
    info = paste0(
      "Exported but not in hal_help bundle: ",
      paste(missing_topics, collapse = ", "),
      ". Add to `topics` in data-raw/build_help.R and rebuild."
    )
  )
})
