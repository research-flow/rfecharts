# RF echarts

A collection of R functions for creating interactive charts using the ECharts library.

## Installation

You can install the released version of RFecharts from GitLab.

```r
remotes::install_gitlab(auth_token = "your_token", repo = "visualisation/rfecharts", host = "http://192.xxx.xx.x")
```

## Usage

The RF ECharts package provides a range of functions for creating different types of charts, including:

`ec_<>()`: Create a chart
`prep_<>()`: Prepare the raw data for plotting.
`example_<>`: Anonymised example data sets for testing.

### Classes

The RF ECharts package includes the following classes:

*   `name_cleaner`: Responsible for cleaning and formatting names for use in echarts4r charts.
*   `color`: Handles color-related operations.

## Examples

Here are some examples of how to use the RF ECharts functions:

```r
example_concentration |> 
 prep_concentration(`Orvosok száma`, value) |> 
 ec_concentration()
 
example_concentration |> 
 prep_concentration(`Orvosok száma`, value) |> 
 ec_concentration(nc = name_cleaner(language = "eng"))
```