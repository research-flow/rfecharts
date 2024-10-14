#' ec_sov
#'
#' Visualize stacked bar chart with optional percentage labels.
#'
#' @description This function creates an echarts4r chart to display a stacked bar chart with optional percentage labels.
#'
#' @param data data.frame containing the data to be visualized.
#' @param x_label string indicating the x-axis label. Default is NULL.
#' @param colors vector of colors to use for the chart. Default is NULL.
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
#' example_sov |> # ! this ec_ does not have an addtional prep_ function
#'   ec_sov(x = csatorna_egyedi, y = valasz_szovege, group = brand, nc = name_cleaner(language = "hun"))


ec_sov <- function(data, x, y, group, x_label = NULL, colors = NULL, nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {

  data <- data |>
    dplyr::select(x = {{ x }}, y = {{ y }}, group = {{ group }})

  if (is.null(theme)) theme <- \(x) x

  # check type for percent
  fill_type <- data |> 
    dplyr::group_by(y) |>
    dplyr::summarise(close_to_one = abs(sum(x) - 1) < .01) |> 
    dplyr::pull(close_to_one) |> 
    all(na.rm = TRUE)

  if (fill_type) {
    x_formatter <- RFecharts::formatter(style = "percent", language = nc@language)
  } else {
    x_formatter <- RFecharts::formatter(language = nc@language)
  }

  # tooltip
  if (fill_type) {
    tooltip <- \(x) e_tooltip(x,
      trigger = "axis",
      valueFormatter =  htmlwidgets::JS("
        (value) => (value*100).toFixed(1) + '%'
      ")
    )
  } else {
    tooltip <- \(x) e_tooltip(x, trigger = "axis")
  }

  # colors
  colors_to_group <- data |> 
    dplyr::distinct(group) |> 
    tidyr::drop_na()

  if (is.null(colors)) {
    colors <- co@get(1:nrow(colors_to_group))
  }

  colors_to_group <- colors_to_group |>
    dplyr::mutate(color = colors)

  if (fill_type) {
    data <- data |> 
      dplyr::mutate(label = nc@percent(x))
  } else {
    data <- data |> 
      dplyr::mutate(label = nc@number(x))
  }

  data |> 
    group_by(group) |> 
    e_charts(x = y, reorder = F) |> # flip at the end
    e_bar(x, stack = "grp", bind = label,
      label = list(
      position = "inside",
      show = TRUE,
      formatter = "{b}"
    )) |>
    e_color(colors) |> 
    e_flip_coords() |>
    tooltip() |> 
    e_x_axis(
      formatter = x_formatter,
      name = x_label, 
      nameLocation = "middle",
      nameGap = 30
    ) |> 
    theme() |> 
    (\(x) if (as_json) echarts4r::e_inspect(x, json = T) else x) ()
}
