% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ec_concentration.R
\name{ec_concentration}
\alias{ec_concentration}
\title{ec_concentration}
\usage{
ec_concentration(
  data,
  nc = RFecharts::name_cleaner(),
  co = RFecharts::color(),
  as_json = shiny::isRunning(),
  theme = RFecharts::rf_echarts_theme
)
}
\arguments{
\item{data}{data.frame containing the data to be visualized.}

\item{nc}{`name_cleaner` object. Default is `RFecharts::name_cleaner()`.}

\item{co}{`color` object. Default is `RFecharts::color()`.}

\item{as_json}{Return as json? By default `shiny::isRunning()`, thus it will appear as chart in your console, but as json in the shiny app.}

\item{theme}{Commonly used RF modification for the end of the chart. Default is `RFecharts::rf_echarts_theme`.}
}
\value{
echarts4r chart
}
\description{
This function creates an echarts4r chart to display the concentration of patients by percentile category.
}
\details{
Visualize concentration of patients by percentile category.
}
\examples{
example_concentration |> 
 prep_concentration(`Orvosok száma`, value) |> 
 ec_concentration()
 
example_concentration |> 
 prep_concentration(`Orvosok száma`, value) |> 
 ec_concentration(nc = name_cleaner(language = "eng"))

}
