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
#'   commas. Common values are `"markdown"`, `"json"`, and `"html"`.
#' @param hybrid Optional hybrid backend name, for example `"docling-fast"`.
#'   Call `opendataloader_pdf_require(extras = "hybrid")` before this when
#'   using hybrid mode.
#' @param hybrid_mode Optional hybrid mode passed through to Python, for
#'   example `"full"`.
#' @param check_java_available If `TRUE`, verify that `java` is on PATH before
#'   calling Python.
#' @param ... Additional keyword arguments passed to
#'   `opendataloader_pdf.convert()`.
#'
#' @return The value returned by `opendataloader_pdf.convert()`.
#' @export
convert_pdf <- function(input_path,
                        output_dir = "output",
                        format = c("markdown", "json"),
                        hybrid = NULL,
                        hybrid_mode = NULL,
                        check_java_available = TRUE,
                        ...) {
  if (isTRUE(check_java_available)) {
    check_java()
  }

  input_path <- normalize_pdf_paths(input_path)
  output_dir <- maybe_normalize_dir(output_dir)
  format <- paste(format, collapse = ",")

  opendataloader_pdf()$convert(
    input_path = as.list(input_path),
    output_dir = output_dir,
    format = format,
    hybrid = hybrid,
    hybrid_mode = hybrid_mode,
    ...
  )
}
