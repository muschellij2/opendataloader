reticulate_import <- function(...) { # nocov start
  reticulate::import(...)
} # nocov end

#' Import the opendataloader_pdf Python module
#'
#' @param delay_load Whether reticulate should delay loading Python until the
#'   module is first used.
#'
#' @return The imported Python module.
#' @export
opendataloader_pdf <- function(delay_load = FALSE) {
  reticulate_import("opendataloader_pdf", delay_load = delay_load)
}

#' Convert PDFs with OpenDataLoader PDF
#'
#' @param input_path Character vector of PDF files or directories.
#' @param output_dir Directory where converted files should be written.
#' @param format Output format string, or a character vector collapsed with
#'   commas. Values are `"json"`, `"text"`, `"html"`, `"pdf"`, `"markdown"`,
#'   `"markdown-with-html"`, `"markdown-with-images"`, and `"tagged-pdf"`.
#' @param password Optional password for encrypted PDFs.
#' @param content_safety_off Optional content safety filters to disable.
#'   Values are `"all"`, `"hidden-text"`, `"off-page"`, `"tiny"`, and
#'   `"hidden-ocg"`.
#' @param sanitize If `TRUE`, enable sensitive data sanitization.
#' @param keep_line_breaks If `TRUE`, preserve original line breaks.
#' @param replace_invalid_chars Optional replacement character for invalid or
#'   unrecognized characters.
#' @param use_struct_tree If `TRUE`, use the PDF structure tree.
#' @param table_method Optional table detection method. Values are `"default"`
#'   and `"cluster"`.
#' @param reading_order Optional reading order algorithm. Values are `"off"`
#'   and `"xycut"`.
#' @param markdown_page_separator,text_page_separator,html_page_separator
#'   Optional page separators. Use `"%page-number%"` for page numbers.
#' @param image_output Optional image output mode. Values are `"off"`,
#'   `"embedded"`, and `"external"`.
#' @param image_format Optional extracted image format. Values are `"png"` and
#'   `"jpeg"`.
#' @param image_dir Optional directory for extracted images.
#' @param pages Optional page range string, for example `"1,3,5-7"`.
#' @param include_header_footer If `TRUE`, include page headers and footers.
#' @param detect_strikethrough If `TRUE`, detect strikethrough text.
#' @param hybrid Optional hybrid backend name, for example `"docling-fast"`.
#'   Call `opendataloader_pdf_require(extras = "hybrid")` before this when
#'   using hybrid mode.
#' @param hybrid_mode Optional hybrid mode passed through to Python. Values are
#'   `"auto"` and `"full"`.
#' @param hybrid_url Optional hybrid backend server URL.
#' @param hybrid_timeout Optional hybrid backend request timeout in
#'   milliseconds. Use `0` for no timeout.
#' @param hybrid_fallback If `TRUE`, opt in to Java fallback on hybrid backend
#'   errors.
#' @param to_stdout If `TRUE`, write output to stdout instead of file. The
#'   upstream converter supports this for single-format output.
#' @param verbose If `TRUE`, run the Python converter in verbose mode. This is
#'   passed to Python as `quiet = !verbose`.
#' @param check_java_available If `TRUE`, verify that `java` is on PATH before
#'   calling Python.
#' @param ... Additional named arguments passed to
#'   `opendataloader_pdf.convert()`. Values must be `NULL`, logical, numeric,
#'   or character vectors.
#'
#' @return An `opendataloader_conversion` object, a list with:
#' \describe{
#'   \item{input_path}{Normalized input paths.}
#'   \item{output_dir}{Normalized output directory, or `NULL`.}
#'   \item{format}{Comma-separated output formats passed to Python.}
#'   \item{output_files}{Files present in `output_dir` after conversion.}
#'   \item{result}{Raw value returned by `opendataloader_pdf.convert()`.}
#'   \item{args}{Additional debug metadata and arguments passed to Python.}
#' }
#' @export
convert_pdf <- function(input_path,
                        output_dir = "output",
                        format = c("markdown", "json"),
                        password = NULL,
                        content_safety_off = NULL,
                        sanitize = FALSE,
                        keep_line_breaks = FALSE,
                        replace_invalid_chars = NULL,
                        use_struct_tree = FALSE,
                        table_method = NULL,
                        reading_order = NULL,
                        markdown_page_separator = NULL,
                        text_page_separator = NULL,
                        html_page_separator = NULL,
                        image_output = NULL,
                        image_format = NULL,
                        image_dir = NULL,
                        pages = NULL,
                        include_header_footer = FALSE,
                        detect_strikethrough = FALSE,
                        hybrid = NULL,
                        hybrid_mode = NULL,
                        hybrid_url = NULL,
                        hybrid_timeout = NULL,
                        hybrid_fallback = FALSE,
                        to_stdout = FALSE,
                        verbose = TRUE,
                        check_java_available = TRUE,
                        ...) {
  if (isTRUE(check_java_available)) {
    check_java()
  }

  input_path <- normalize_pdf_paths(input_path)
  output_dir <- maybe_normalize_dir(output_dir)
  format <- check_choices(
    format,
    "format",
    c("json", "text", "html", "pdf", "markdown", "markdown-with-html",
      "markdown-with-images", "tagged-pdf"),
    multiple = TRUE
  )
  format <- paste(format, collapse = ",")
  password <- check_optional_string(password, "password")
  content_safety_off <- check_choices(
    content_safety_off,
    "content_safety_off",
    c("all", "hidden-text", "off-page", "tiny", "hidden-ocg"),
    multiple = TRUE
  )
  content_safety_off <- if (is.null(content_safety_off)) {
    NULL
  } else {
    paste(content_safety_off, collapse = ",")
  }
  sanitize <- check_bool(sanitize, "sanitize")
  keep_line_breaks <- check_bool(keep_line_breaks, "keep_line_breaks")
  replace_invalid_chars <- check_optional_string(
    replace_invalid_chars,
    "replace_invalid_chars"
  )
  use_struct_tree <- check_bool(use_struct_tree, "use_struct_tree")
  table_method <- check_choices(table_method, "table_method", c("default", "cluster"))
  reading_order <- check_choices(reading_order, "reading_order", c("off", "xycut"))
  markdown_page_separator <- check_optional_string(
    markdown_page_separator,
    "markdown_page_separator"
  )
  text_page_separator <- check_optional_string(text_page_separator, "text_page_separator")
  html_page_separator <- check_optional_string(html_page_separator, "html_page_separator")
  image_output <- check_choices(image_output, "image_output", c("off", "embedded", "external"))
  image_format <- check_choices(image_format, "image_format", c("png", "jpeg"))
  image_dir <- maybe_normalize_dir(image_dir)
  pages <- check_optional_string(pages, "pages")
  include_header_footer <- check_bool(include_header_footer, "include_header_footer")
  detect_strikethrough <- check_bool(detect_strikethrough, "detect_strikethrough")
  hybrid <- check_optional_string(hybrid, "hybrid")
  hybrid_mode <- check_choices(hybrid_mode, "hybrid_mode", c("auto", "full"))
  hybrid_url <- check_optional_string(hybrid_url, "hybrid_url")
  hybrid_timeout <- check_hybrid_timeout(hybrid_timeout)
  hybrid_fallback <- check_bool(hybrid_fallback, "hybrid_fallback")
  to_stdout <- check_bool(to_stdout, "to_stdout")
  verbose <- check_bool(verbose, "verbose")
  dots <- validate_convert_dots(
    list(...),
    reserved = c(
      "input_path", "output_dir", "format", "password",
      "content_safety_off", "sanitize", "keep_line_breaks",
      "replace_invalid_chars", "use_struct_tree", "table_method",
      "reading_order", "markdown_page_separator", "text_page_separator",
      "html_page_separator", "image_output", "image_format", "image_dir",
      "pages", "include_header_footer", "detect_strikethrough", "hybrid",
      "hybrid_mode", "hybrid_url", "hybrid_timeout", "hybrid_fallback",
      "to_stdout", "verbose", "check_java_available"
    )
  )
  if ("quiet" %in% names(dots)) {
    stop("Use `verbose`, not `quiet`, when calling `convert_pdf()`.",
         call. = FALSE)
  }
  options <- list(
    password = password,
    content_safety_off = content_safety_off,
    sanitize = sanitize,
    keep_line_breaks = keep_line_breaks,
    replace_invalid_chars = replace_invalid_chars,
    use_struct_tree = use_struct_tree,
    table_method = table_method,
    reading_order = reading_order,
    markdown_page_separator = markdown_page_separator,
    text_page_separator = text_page_separator,
    html_page_separator = html_page_separator,
    image_output = image_output,
    image_format = image_format,
    image_dir = image_dir,
    pages = pages,
    include_header_footer = include_header_footer,
    detect_strikethrough = detect_strikethrough,
    hybrid = hybrid,
    hybrid_mode = hybrid_mode,
    hybrid_url = hybrid_url,
    hybrid_timeout = hybrid_timeout,
    hybrid_fallback = hybrid_fallback,
    to_stdout = to_stdout,
    quiet = !verbose
  )

  result <- do.call(
    opendataloader_pdf()$convert,
    c(
      list(
        input_path = as.list(input_path),
        output_dir = output_dir,
        format = format
      ),
      options,
      dots
    )
  )

  new_opendataloader_conversion(
    input_path = as.list(input_path),
    output_dir = output_dir,
    format = format,
    options = options,
    verbose = verbose,
    result = result,
    dots = dots
  )
}

new_opendataloader_conversion <- function(input_path,
                                          output_dir,
                                          format,
                                          options,
                                          verbose,
                                          result,
                                          dots = list()) {
  structure(
    list(
      input_path = unlist(input_path, use.names = FALSE),
      output_dir = output_dir,
      format = format,
      output_files = list_output_files(output_dir),
      result = result,
      args = list(
        verbose = verbose,
        quiet = !verbose,
        options = options,
        dots = dots
      )
    ),
    class = "opendataloader_conversion"
  )
}

list_output_files <- function(output_dir) {
  if (is.null(output_dir) || !dir.exists(output_dir)) {
    return(character())
  }
  normalizePath(
    list.files(output_dir, recursive = TRUE, full.names = TRUE, no.. = TRUE),
    mustWork = FALSE
  )
}

#' @export
print.opendataloader_conversion <- function(x, ...) {
  cat("OpenDataLoader PDF conversion\n")
  cat("  Inputs: ", length(x$input_path), "\n", sep = "")
  cat("  Output directory: ", x$output_dir %||% "<none>", "\n", sep = "")
  cat("  Formats: ", x$format, "\n", sep = "")
  cat("  Output files: ", length(x$output_files), "\n", sep = "")
  invisible(x)
}
