formatter <- function(..., language = NULL) {
  if (is.null(language)) {
    if (exists("nc", envir = parent.frame())) {
      language <- get("nc", envir = parent.frame())@language
    } else {
      language <- "hun"
    }
  }
  suppressWarnings({
    echarts4r::e_axis_formatter(..., locale = language)
  })
}

