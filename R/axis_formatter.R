#' formatter
#' 
#' @description
#' Wrapper for the e_axis_formatter() to suppress warnings and set the language.
#' 
#' @param language  Language. Preferably from `RFecharts::name_cleaner()@language`. Default is "hun".
#' @param ... Additional parameters passed to `echarts4r::e_axis_formatter()`.
#' @return echarts4r chart
#' @export
#'

formatter <- function(language = "hun", ...) {
  suppressWarnings({
    echarts4r::e_axis_formatter(..., locale = language)
  })
}

