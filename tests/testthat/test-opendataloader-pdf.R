test_that("opendataloader_pdf imports the Python module", {
  local_mocked_bindings(
    reticulate_import = function(module, delay_load = FALSE) {
      list(module = module, delay_load = delay_load)
    }
  )

  expect_equal(
    opendataloader_pdf(delay_load = TRUE),
    list(module = "opendataloader_pdf", delay_load = TRUE)
  )
})

test_that("convert_pdf normalizes and forwards conversion arguments", {
  input <- tempfile(fileext = ".pdf")
  writeLines("not a real pdf", input)
  output <- tempdir()

  calls <- list()
  fake_module <- list(
    convert = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      "converted"
    }
  )

  local_mocked_bindings(
    check_java = function() TRUE,
    opendataloader_pdf = function(delay_load = FALSE) fake_module
  )

  expect_equal(
    convert_pdf(
      input_path = input,
      output_dir = output,
      format = c("markdown", "json"),
      hybrid = "docling-fast",
      hybrid_mode = "full",
      quiet = TRUE
    ),
    "converted"
  )

  expect_equal(calls[[1L]]$input_path, as.list(normalizePath(input)))
  expect_equal(calls[[1L]]$output_dir, normalizePath(output))
  expect_equal(calls[[1L]]$format, "markdown,json")
  expect_equal(calls[[1L]]$hybrid, "docling-fast")
  expect_equal(calls[[1L]]$hybrid_mode, "full")
  expect_true(calls[[1L]]$quiet)
})

test_that("convert_pdf can skip Java preflight", {
  input <- tempfile(fileext = ".pdf")
  writeLines("not a real pdf", input)

  local_mocked_bindings(
    check_java = function() stop("should not run"),
    opendataloader_pdf = function(delay_load = FALSE) {
      list(convert = function(...) "converted")
    }
  )

  expect_equal(
    convert_pdf(input, output_dir = NULL, check_java_available = FALSE),
    "converted"
  )
})
