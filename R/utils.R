`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

normalize_pdf_paths <- function(x, must_work = TRUE) {
  if (!is.character(x) || !length(x) || !all(nzchar(x))) {
    stop("`input_path` must be a character vector of file or directory paths.",
         call. = FALSE)
  }
  normalizePath(x, mustWork = must_work)
}

maybe_normalize_dir <- function(x, must_work = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
    stop("`output_dir` must be a single non-empty string.", call. = FALSE)
  }
  normalizePath(x, mustWork = must_work)
}
