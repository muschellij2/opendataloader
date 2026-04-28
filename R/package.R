#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) { # nocov start
  opendataloader_pdf_require()
} # nocov end
