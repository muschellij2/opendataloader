# opendataloader

<!-- badges: start -->
[![R-CMD-check](https://github.com/muschellij2/opendataloader/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/muschellij2/opendataloader/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/muschellij2/opendataloader/graph/badge.svg)](https://app.codecov.io/gh/muschellij2/opendataloader)
<!-- badges: end -->
  
R interface to the Python
[`opendataloader-pdf`](https://github.com/opendataloader-project/opendataloader-pdf)
package using reticulate.

```r
install.packages("reticulate")

# From this package:
library(opendataloader)

convert_pdf(
  input_path = c("file1.pdf", "file2.pdf"),
  output_dir = "output",
  format = c("markdown", "json")
)
```

The package declares `opendataloader-pdf` with `reticulate::py_require()` when
it is loaded. To request optional Python extras before Python is initialized:

```r
opendataloader_pdf_require(extras = "hybrid")

convert_pdf(
  input_path = "file1.pdf",
  output_dir = "output",
  hybrid = "docling-fast"
)
```

The upstream Python package requires Python 3.10 or newer and Java 11 or newer.
