test_that("%||% returns fallback only for NULL", {
  expect_equal(opendataloader:::`%||%`(NULL, "fallback"), "fallback")
  expect_null(opendataloader:::`%||%`(NULL, NULL))
  expect_equal(opendataloader:::`%||%`("value", "fallback"), "value")
})

test_that("normalize_pdf_paths validates input paths", {
  path <- tempfile(fileext = ".pdf")
  writeLines("not a real pdf", path)

  expect_equal(opendataloader:::normalize_pdf_paths(path), normalizePath(path))
  expect_error(opendataloader:::normalize_pdf_paths(character()), "`input_path`")
  expect_error(opendataloader:::normalize_pdf_paths(""), "`input_path`")
  expect_error(opendataloader:::normalize_pdf_paths(c(path, "")), "`input_path`")
})

test_that("maybe_normalize_dir validates output directories", {
  dir <- tempdir()

  expect_null(opendataloader:::maybe_normalize_dir(NULL))
  expect_equal(opendataloader:::maybe_normalize_dir(dir), normalizePath(dir))
  expect_error(opendataloader:::maybe_normalize_dir(character()), "`output_dir`")
  expect_error(opendataloader:::maybe_normalize_dir(c("a", "b")), "`output_dir`")
  expect_error(opendataloader:::maybe_normalize_dir(""), "`output_dir`")
})
