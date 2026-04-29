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

check_bool <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  x
}

check_optional_string <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be `NULL` or a single non-empty string.",
         call. = FALSE)
  }
  x
}

check_optional_character <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || !length(x) || anyNA(x) || any(!nzchar(x))) {
    stop("`", name, "` must be `NULL` or a non-empty character vector.",
         call. = FALSE)
  }
  x
}

check_choices <- function(x, name, choices, multiple = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }
  if (multiple) {
    x <- check_optional_character(x, name)
  } else {
    x <- check_optional_string(x, name)
  }
  bad <- setdiff(x, choices)
  if (length(bad)) {
    stop(
      "`", name, "` must be one of: ",
      paste(choices, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  x
}

check_hybrid_timeout <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 0) {
    return(as.character(x))
  }
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    return(x)
  }
  stop("`hybrid_timeout` must be `NULL`, a non-negative number, or a single string.",
       call. = FALSE)
}

validate_convert_dots <- function(dots, reserved = character()) {
  if (!length(dots)) {
    return(dots)
  }
  dot_names <- names(dots)
  if (is.null(dot_names) || any(!nzchar(dot_names))) {
    stop("All arguments passed through `...` must be named.", call. = FALSE)
  }
  conflicts <- intersect(dot_names, reserved)
  if (length(conflicts)) {
    stop(
      "Do not pass explicit `convert_pdf()` arguments through `...`: ",
      paste(conflicts, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  unsupported <- vapply(
    dots,
    function(x) {
      !(is.null(x) ||
          is.logical(x) ||
          is.numeric(x) ||
          is.character(x))
    },
    logical(1)
  )
  if (any(unsupported)) {
    stop(
      "Arguments passed through `...` must be `NULL`, logical, numeric, or character values: ",
      paste(dot_names[unsupported], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  dots
}
