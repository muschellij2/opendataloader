reticulate_py_require <- function(...) { # nocov start
  reticulate::py_require(...)
} # nocov end

opendataloader_pdf_requirement <- function(extras = NULL, version = NULL) {
  extras <- unique(extras %||% character())
  if (!all(nzchar(extras))) {
    stop("`extras` must contain non-empty strings.", call. = FALSE)
  }

  requirement <- "opendataloader-pdf"
  if (length(extras)) {
    requirement <- sprintf("%s[%s]", requirement, paste(extras, collapse = ","))
  }
  if (!is.null(version)) {
    if (!is.character(version) || length(version) != 1 || !nzchar(version)) {
      stop("`version` must be a single non-empty string.", call. = FALSE)
    }
    requirement <- paste0(requirement, version)
  }
  requirement
}

#' Declare Python requirements for OpenDataLoader PDF
#'
#' Declares the `opendataloader-pdf` Python package for reticulate's managed
#' Python environment. Call this before Python is initialized when you need
#' optional extras such as `"hybrid"` or a stricter package version.
#'
#' @param extras Optional Python extras, for example `"hybrid"`.
#' @param version Optional pip-style version constraint, for example
#'   `">=0.1.0"`.
#' @param python_version Python version constraint. The upstream project
#'   requires Python 3.10 or newer.
#' @param exclude_newer Optional date or timestamp passed to
#'   [reticulate::py_require()].
#' @param action Requirement action passed to [reticulate::py_require()].
#'
#' @return Invisibly returns `NULL`.
#' @export
opendataloader_pdf_require <- function(extras = NULL,
                                       version = NULL,
                                       python_version = ">=3.10",
                                       exclude_newer = NULL,
                                       action = c("add", "remove", "set")) {
  action <- match.arg(action)
  reticulate_py_require(
    packages = opendataloader_pdf_requirement(extras = extras, version = version),
    python_version = python_version,
    exclude_newer = exclude_newer,
    action = action
  )
  invisible(NULL)
}

#' @rdname opendataloader_pdf_require
#' @export
py_require_opendataloader_pdf <- opendataloader_pdf_require

#' Show declared Python requirements
#'
#' @return The current reticulate Python requirements manifest.
#' @export
opendataloader_python_requirements <- function() {
  reticulate_py_require()
}
