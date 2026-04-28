test_that("opendataloader_pdf_requirement builds pip requirement strings", {
  expect_equal(opendataloader:::opendataloader_pdf_requirement(), "opendataloader-pdf")
  expect_equal(
    opendataloader:::opendataloader_pdf_requirement(extras = c("hybrid", "hybrid")),
    "opendataloader-pdf[hybrid]"
  )
  expect_equal(
    opendataloader:::opendataloader_pdf_requirement(
      extras = c("hybrid", "gpu"),
      version = ">=0.1.0"
    ),
    "opendataloader-pdf[hybrid,gpu]>=0.1.0"
  )
})

test_that("opendataloader_pdf_requirement validates inputs", {
  expect_error(
    opendataloader:::opendataloader_pdf_requirement(extras = ""),
    "`extras` must contain non-empty strings",
    fixed = TRUE
  )
  expect_error(
    opendataloader:::opendataloader_pdf_requirement(version = c(">=1", "<2")),
    "`version` must be a single non-empty string",
    fixed = TRUE
  )
  expect_error(
    opendataloader:::opendataloader_pdf_requirement(version = ""),
    "`version` must be a single non-empty string",
    fixed = TRUE
  )
})

test_that("opendataloader_pdf_require delegates to py_require", {
  calls <- list()
  local_mocked_bindings(
    reticulate_py_require = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      NULL
    }
  )

  expect_invisible(
    opendataloader_pdf_require(
      extras = "hybrid",
      version = ">=0.1.0",
      python_version = ">=3.11",
      exclude_newer = "2026-01-01",
      action = "set"
    )
  )

  expect_equal(calls[[1L]]$packages, "opendataloader-pdf[hybrid]>=0.1.0")
  expect_equal(calls[[1L]]$python_version, ">=3.11")
  expect_equal(calls[[1L]]$exclude_newer, "2026-01-01")
  expect_equal(calls[[1L]]$action, "set")
})

test_that("opendataloader_python_requirements returns the reticulate manifest", {
  local_mocked_bindings(
    reticulate_py_require = function(...) {
      list(packages = "opendataloader-pdf", python_version = ">=3.10")
    }
  )

  expect_equal(
    opendataloader_python_requirements(),
    list(packages = "opendataloader-pdf", python_version = ">=3.10")
  )
})
