#' ec_concentration
#'
#' Visualize concentration of patients by percentile category.
#'
#' @description This function creates an echarts4r chart to display the concentration of patients by percentile category.
#'
#' @param data data.frame containing the data to be visualized.
#' @param nc `name_cleaner` object. Default is `RFecharts::name_cleaner()`.
#' @param co `color` object. Default is `RFecharts::color()`.
#' @param as_json Return as json? By default `shiny::isRunning()`, thus it will appear as chart in your console, but as json in the shiny app.
#' @param theme Commonly used RF modification for the end of the chart. Default is `RFecharts::rf_echarts_theme`.
#'
#' @return echarts4r chart
#'
#' @export
#'
#' @examples
#' example_concentration |> 
#'  prep_concentration(`Orvosok száma`, value) |> 
#'  ec_concentration()
#'  
#' example_concentration |> 
#'  prep_concentration(`Orvosok száma`, value) |> 
#'  ec_concentration(nc = name_cleaner(language = "eng"))
#' 

ec_concentration <- function(data, nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {
  if (is.null(theme)) theme <- \(x) x

  data |> 
    echarts4r::e_charts(x = percentile_category, reorder = F) |>
    echarts4r::e_bar(cumn_patient,
          bind = percent_patient,
          name = nc@prettify("cumn_patient"),
          color = co@get(1, .2),
          stack = "grp"
    ) |>
    echarts4r::e_area(
      n_patient,
      name = nc@prettify("n_patient"),
      color = co@get(5)
    ) |>
    echarts4r::e_line(
      patient_per_doctor,
      name = nc@prettify("patient_per_doctor"),
      color = co@get(2),
      y_index = 1
    ) |>
    echarts4r::e_x_axis(name = nc@prettify("percentile"),
             nameLocation = "middle",
             nameGap = 30) |>
    echarts4r::e_y_axis(alignTicks = TRUE) |>
    echarts4r::e_y_axis(index = 1,
             name = nc@prettify("patient_per_doctor")) |>
    echarts4r::e_y_axis(index = 0, formatter = RFecharts::formatter(language = nc@language),
           name = nc@prettify("n_patient")) |>
    echarts4r::e_tooltip(
      trigger = "axis"
    ) |>
    theme() |>
    (\(x) if (as_json) echarts4r::e_inspect(x, json = T) else x) ()
}
