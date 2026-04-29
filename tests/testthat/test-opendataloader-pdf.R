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
  output <- tempfile("opendataloader-output-")
  dir.create(output)

  calls <- list()
  fake_module <- list(
    convert = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      writeLines("converted", file.path(output, "example.md"))
      list(status = "converted")
    }
  )

  local_mocked_bindings(
    check_java = function() TRUE,
    opendataloader_pdf = function(delay_load = FALSE) fake_module
  )

  converted <- convert_pdf(
    input_path = input,
    output_dir = output,
    format = c("markdown", "json"),
    password = "secret",
    content_safety_off = c("hidden-text", "tiny"),
    sanitize = TRUE,
    keep_line_breaks = TRUE,
    replace_invalid_chars = "?",
    use_struct_tree = TRUE,
    table_method = "cluster",
    reading_order = "xycut",
    markdown_page_separator = "\n<!-- page %page-number% -->\n",
    text_page_separator = "\n--- page %page-number% ---\n",
    html_page_separator = "<hr>",
    image_output = "external",
    image_format = "png",
    image_dir = output,
    pages = "1,3,5-7",
    include_header_footer = TRUE,
    detect_strikethrough = TRUE,
    hybrid = "docling-fast",
    hybrid_mode = "full",
    hybrid_url = "http://127.0.0.1:5002",
    hybrid_timeout = 1000,
    hybrid_fallback = TRUE,
    to_stdout = FALSE,
    verbose = FALSE,
    extra_arg = TRUE
  )

  expect_s3_class(converted, "opendataloader_conversion")
  expect_equal(converted$input_path, normalizePath(input))
  expect_equal(converted$output_dir, normalizePath(output))
  expect_equal(converted$format, "markdown,json")
  expect_equal(converted$output_files, normalizePath(file.path(output, "example.md")))
  expect_equal(converted$result, list(status = "converted"))
  expect_equal(converted$args$options$hybrid, "docling-fast")
  expect_equal(converted$args$options$hybrid_mode, "full")
  expect_false(converted$args$verbose)
  expect_true(converted$args$quiet)
  expect_true(converted$args$dots$extra_arg)

  expect_equal(calls[[1L]]$input_path, as.list(normalizePath(input)))
  expect_equal(calls[[1L]]$output_dir, normalizePath(output))
  expect_equal(calls[[1L]]$format, "markdown,json")
  expect_equal(calls[[1L]]$password, "secret")
  expect_equal(calls[[1L]]$content_safety_off, "hidden-text,tiny")
  expect_true(calls[[1L]]$sanitize)
  expect_true(calls[[1L]]$keep_line_breaks)
  expect_equal(calls[[1L]]$replace_invalid_chars, "?")
  expect_true(calls[[1L]]$use_struct_tree)
  expect_equal(calls[[1L]]$table_method, "cluster")
  expect_equal(calls[[1L]]$reading_order, "xycut")
  expect_equal(calls[[1L]]$markdown_page_separator, "\n<!-- page %page-number% -->\n")
  expect_equal(calls[[1L]]$text_page_separator, "\n--- page %page-number% ---\n")
  expect_equal(calls[[1L]]$html_page_separator, "<hr>")
  expect_equal(calls[[1L]]$image_output, "external")
  expect_equal(calls[[1L]]$image_format, "png")
  expect_equal(calls[[1L]]$image_dir, normalizePath(output))
  expect_equal(calls[[1L]]$pages, "1,3,5-7")
  expect_true(calls[[1L]]$include_header_footer)
  expect_true(calls[[1L]]$detect_strikethrough)
  expect_equal(calls[[1L]]$hybrid, "docling-fast")
  expect_equal(calls[[1L]]$hybrid_mode, "full")
  expect_equal(calls[[1L]]$hybrid_url, "http://127.0.0.1:5002")
  expect_equal(calls[[1L]]$hybrid_timeout, "1000")
  expect_true(calls[[1L]]$hybrid_fallback)
  expect_false(calls[[1L]]$to_stdout)
  expect_true(calls[[1L]]$quiet)
  expect_true(calls[[1L]]$extra_arg)
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
    convert_pdf(input, output_dir = NULL, check_java_available = FALSE)$result,
    "converted"
  )
})

test_that("convert_pdf passes default verbose mode as quiet false", {
  input <- tempfile(fileext = ".pdf")
  writeLines("not a real pdf", input)
  calls <- list()

  local_mocked_bindings(
    check_java = function() TRUE,
    opendataloader_pdf = function(delay_load = FALSE) {
      list(convert = function(...) {
        calls[[length(calls) + 1L]] <<- list(...)
        "converted"
      })
    }
  )

  converted <- convert_pdf(input, output_dir = NULL)

  expect_true(converted$args$verbose)
  expect_false(converted$args$quiet)
  expect_false(calls[[1L]]$quiet)
})

test_that("convert_pdf validates verbose and rejects direct quiet", {
  input <- tempfile(fileext = ".pdf")
  writeLines("not a real pdf", input)

  local_mocked_bindings(
    check_java = function() TRUE,
    opendataloader_pdf = function(delay_load = FALSE) {
      list(convert = function(...) "converted")
    }
  )

  expect_error(convert_pdf(input, output_dir = NULL, verbose = NA), "`verbose`")
  expect_error(
    convert_pdf(input, output_dir = NULL, quiet = TRUE),
    "Use `verbose`, not `quiet`",
    fixed = TRUE
  )
})

test_that("convert_pdf validates explicit option types and choices", {
  input <- tempfile(fileext = ".pdf")
  writeLines("not a real pdf", input)

  local_mocked_bindings(
    check_java = function() TRUE,
    opendataloader_pdf = function(delay_load = FALSE) {
      list(convert = function(...) "converted")
    }
  )

  expect_error(convert_pdf(input, output_dir = NULL, format = "docx"), "`format`")
  expect_error(convert_pdf(input, output_dir = NULL, password = 1), "`password`")
  expect_error(
    convert_pdf(input, output_dir = NULL, content_safety_off = "bad"),
    "`content_safety_off`"
  )
  expect_error(convert_pdf(input, output_dir = NULL, sanitize = NA), "`sanitize`")
  expect_error(
    convert_pdf(input, output_dir = NULL, keep_line_breaks = "yes"),
    "`keep_line_breaks`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, replace_invalid_chars = c("a", "b")),
    "`replace_invalid_chars`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, use_struct_tree = NA),
    "`use_struct_tree`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, table_method = "ml"),
    "`table_method`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, reading_order = "unknown"),
    "`reading_order`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, image_output = "inline"),
    "`image_output`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, image_format = "gif"),
    "`image_format`"
  )
  expect_error(convert_pdf(input, output_dir = NULL, pages = 1), "`pages`")
  expect_error(
    convert_pdf(input, output_dir = NULL, include_header_footer = NA),
    "`include_header_footer`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, detect_strikethrough = NA),
    "`detect_strikethrough`"
  )
  expect_error(convert_pdf(input, output_dir = NULL, hybrid = 1), "`hybrid`")
  expect_error(
    convert_pdf(input, output_dir = NULL, hybrid_mode = "partial"),
    "`hybrid_mode`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, hybrid_timeout = -1),
    "`hybrid_timeout`"
  )
  expect_error(
    convert_pdf(input, output_dir = NULL, hybrid_fallback = NA),
    "`hybrid_fallback`"
  )
  expect_error(convert_pdf(input, output_dir = NULL, to_stdout = NA), "`to_stdout`")
})

test_that("convert_pdf validates passthrough dot arguments", {
  input <- tempfile(fileext = ".pdf")
  writeLines("not a real pdf", input)

  local_mocked_bindings(
    check_java = function() TRUE,
    opendataloader_pdf = function(delay_load = FALSE) {
      list(convert = function(...) "converted")
    }
  )

  expect_error(opendataloader:::validate_convert_dots(list(TRUE)), "must be named")
  expect_error(
    convert_pdf(input, output_dir = NULL, future_arg = list(1)),
    "must be `NULL`, logical, numeric, or character"
  )
})

test_that("list_output_files handles missing output directories", {
  expect_equal(opendataloader:::list_output_files(NULL), character())
  expect_equal(opendataloader:::list_output_files(tempfile()), character())
})

test_that("opendataloader conversion objects print a concise summary", {
  converted <- opendataloader:::new_opendataloader_conversion(
    input_path = list("input.pdf"),
    output_dir = NULL,
    format = "markdown",
    options = list(),
    verbose = TRUE,
    result = list(ok = TRUE),
    dots = list()
  )

  expect_output(print(converted), "OpenDataLoader PDF conversion")
  expect_output(print(converted), "Output files: 0")
  printed <- NULL
  capture.output(printed <- print(converted))
  expect_identical(printed, converted)
})
