# Tests for hal_excel -- focus on the pure pieces: the verification oracle
# (.hal_excel_compare) and the helpers. No LLM, no Excel file required.

test_that("column letters use bijective base-26", {
  expect_equal(.hal_excel_col_letter(1L), "A")
  expect_equal(.hal_excel_col_letter(26L), "Z")
  expect_equal(.hal_excel_col_letter(27L), "AA")
  expect_equal(.hal_excel_col_letter(28L), "AB")
  expect_equal(.hal_excel_col_letter(52L), "AZ")
  expect_equal(.hal_excel_col_letter(53L), "BA")
})

test_that("dtype maps an R vector to a comparison label", {
  expect_equal(.hal_excel_dtype(c(1, 2, 3)), "numeric")
  expect_equal(.hal_excel_dtype(c("a", "b")), "character")
  expect_equal(.hal_excel_dtype(c(TRUE, FALSE)), "logical")
  expect_equal(.hal_excel_dtype(as.Date("2026-01-01")), "date")
})

test_that("clean_code strips a leading .data pipe", {
  expect_equal(.hal_excel_clean_code(".data |> mutate(x = a + b)"),
               "mutate(x = a + b)")
  expect_equal(.hal_excel_clean_code(".data %>% mutate(x = a)"), "mutate(x = a)")
  expect_equal(.hal_excel_clean_code("  mutate(x = a)  "), "mutate(x = a)")
})

test_that("oracle passes when numeric output matches within tolerance", {
  cmp <- .hal_excel_compare(c(2, 4, 6), c(2, 4, 6), "numeric", 1e-9)
  expect_true(cmp$pass)
  expect_equal(cmp$n_match, 3L)
  expect_null(cmp$mism)
})

test_that("oracle tolerates float noise but catches real differences", {
  expect_true(.hal_excel_compare(c(1 + 1e-12), c(1), "numeric", 1e-9)$pass)
  bad <- .hal_excel_compare(c(1, 2, 99), c(1, 2, 3), "numeric", 1e-9)
  expect_false(bad$pass)
  expect_equal(bad$n_match, 2L)
  expect_equal(bad$mism$row, 3L + 1L)        # +1: data starts on sheet row 2
  expect_equal(bad$mism$excel, "3")
  expect_equal(bad$mism$r, "99")
})

test_that("oracle handles NA alignment", {
  expect_true(.hal_excel_compare(c(1, NA, 3), c(1, NA, 3), "numeric", 1e-9)$pass)
  expect_false(.hal_excel_compare(c(1, NA, 3), c(1, 2, 3), "numeric", 1e-9)$pass)
})

test_that("oracle flags length mismatch without erroring", {
  cmp <- .hal_excel_compare(c(1, 2), c(1, 2, 3), "numeric", 1e-9)
  expect_false(cmp$pass)
  expect_match(cmp$error, "length mismatch")
})

test_that("oracle compares character (trimmed), logical, and date", {
  expect_true(.hal_excel_compare(c("a ", " b"), c("a", "b"), "character", 0)$pass)
  expect_true(.hal_excel_compare(c(TRUE, FALSE), c(TRUE, FALSE), "logical", 0)$pass)
  d <- as.Date(c("2026-01-01", "2026-02-01"))
  expect_true(.hal_excel_compare(d, d, "date", 0)$pass)
})

test_that("mismatch report includes counts and rows", {
  cmp <- .hal_excel_compare(c(1, 99), c(1, 2), "numeric", 1e-9)
  report <- .hal_excel_mismatch_report(cmp)
  expect_match(report, "did not match Excel")
  expect_match(report, "Excel=2")
  expect_match(report, "you=99")
})

test_that("system prompt carries the letter->name map and columns", {
  sp <- .hal_excel_system_prompt(c(A = "qty", B = "price"), c("qty", "price"))
  expect_match(sp, "A=qty")
  expect_match(sp, "B=price")
  expect_match(sp, "qty, price")
  expect_match(sp, "mutate")
})

test_that("result coverage shows a dash when match count is unknown", {
  spec <- list(name = "x", formula = "=A2")
  expect_equal(.hal_excel_row(spec, "code", "verified", 4L, 4L, NA)$coverage,
               "4/4")
  expect_equal(
    .hal_excel_row(spec, "code", "unverifiable", NA_integer_, 4L, "no cache")$coverage,
    "-/4"
  )
})

test_that("assemble builds a read line + verified pipeline + commented rest", {
  mk <- function(name, formula, code, status, nm, nt, note) {
    .hal_excel_row(list(name = name, formula = formula), code, status, nm, nt, note)
  }
  meta <- rbind(
    mk("total", "=A2*B2", "dplyr::mutate(total = qty * price)", "verified", 8L, 8L, NA),
    mk("margin", "=(B2-C2)/B2", "dplyr::mutate(margin = (price - cost)/price)",
       "verified", 8L, 8L, NA),
    mk("bad", "=X1", "dplyr::mutate(bad = nope)", "unverified", 3L, 8L, "mismatch")
  )
  code <- .hal_excel_assemble("../model.xlsx", "Sheet1", c("qty", "price", "cost"), meta)

  expect_s3_class(code, "hal_excel_code")
  expect_match(code,
               'openxlsx2::wb_to_df("../model.xlsx", sheet = "Sheet1", col_names = TRUE)',
               fixed = TRUE)
  expect_match(code, '[c("qty", "price", "cost")]', fixed = TRUE)
  expect_match(code, "result <- data |>", fixed = TRUE)
  expect_match(code, "dplyr::mutate(total = qty * price) |>", fixed = TRUE)
  expect_match(code, "Not verified", fixed = TRUE)
  expect_match(code, "# dplyr::mutate(bad = nope)", fixed = TRUE)
})

test_that("hal_excel aborts cleanly on a missing file", {
  skip_if_not_installed("openxlsx2")
  expect_error(hal_excel(tempfile(fileext = ".xlsx")), "not found")
})
