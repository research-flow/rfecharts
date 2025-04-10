% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_cleaner.R
\name{name_cleaner}
\alias{name_cleaner}
\title{name_cleaner Class}
\usage{
name_cleaner(keys, language = "hun")
}
\arguments{
\item{keys}{data.frame containing the mapping of code names to pretty names. Default is a built-in data frame.}

\item{language}{The language to use for formatting and translation. Default is "hun".}
}
\value{
name_cleaner object
}
\description{
This class provides a way to work with names in your application, including formatting and translating names.
}
\details{
The name cleaner class provides a way to clean and format names for use in echarts4r charts.
}
\section{Methods}{

  \describe{
    \item{\code{prettify}}{Format a name for display.}
    \item{\code{percent}}{Format a percentage value for display.}
    \item{\code{number}}{Format a number value for display.}
  }
}

\section{Properties}{

  \describe{
    \item{\code{language}}{The language to use for formatting and translation.}
    \item{\code{keys}}{A data frame containing the mapping of code names to pretty names.}
    \item{\code{pretty_name_col}}{The column name in the keys data frame that contains the pretty names.}
    \item{\code{dec.mark}}{The decimal mark to use for formatting numbers.}
    \item{\code{big.mark}}{The big mark to use for formatting numbers.}
  }
}

\examples{
nc <- RFecharts::name_cleaner(language = "eng")
nc@prettify("cumn_patient")
nc@pnumber(500123.23)

nc <- RFecharts::name_cleaner(language = "hun")
nc@prettify("cumn_patient")
nc@pnumber(500123.23)

}
