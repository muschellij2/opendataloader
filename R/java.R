#' Check whether Java is available
#'
#' `opendataloader-pdf` requires Java 11 or newer at runtime.
#'
#' @return Invisibly returns `TRUE` when `java -version` reports Java 11 or newer.
#' @export
check_java <- function() {
  java <- find_java()
  if (!nzchar(java)) {
    stop("Java was not found on PATH. Install Java 11 or newer before converting PDFs.",
         call. = FALSE)
  }

  result <- java_version(java)
  status <- attr(result, "status") %||% 0L
  if (!identical(status, 0L)) {
    stop("`java -version` failed. Install or configure Java 11 or newer.",
         call. = FALSE)
  }

  major_version <- java_major_version(result)
  if (is.na(major_version)) {
    stop("Could not determine the Java version from `java -version`.",
         call. = FALSE)
  }
  if (major_version < 11) {
    stop("Java 11 or newer is required. Found Java ", major_version, ".",
         call. = FALSE)
  }

  invisible(TRUE)
}

find_java <- function() { # nocov start
  Sys.which("java")
} # nocov end

java_version <- function(java) { # nocov start
  system2(java, "-version", stdout = TRUE, stderr = TRUE)
} # nocov end

java_major_version <- function(version_output) {
  version_output <- paste(version_output, collapse = "\n")
  match <- regexpr('version "([^"]+)"', version_output)
  if (match < 0) {
    return(NA_integer_)
  }

  version <- sub('^version "([^"]+)".*$', "\\1", regmatches(version_output, match))
  parts <- strsplit(version, "[._-]", fixed = FALSE)[[1]]
  if (!length(parts) || !grepl("^[0-9]+$", parts[[1]])) {
    return(NA_integer_)
  }

  if (parts[[1]] == "1" && length(parts) >= 2 && grepl("^[0-9]+$", parts[[2]])) {
    return(as.integer(parts[[2]]))
  }

  as.integer(parts[[1]])
}
