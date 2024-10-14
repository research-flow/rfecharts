#' ec_means
#' 
#' @description
#' Visualize means of different groups (brands) by several y values (Qs).
#' 
#' @param data data.frame
#' @param y Variable to plot on y axis.
#' @param x Variable to plot on x axis.
#' @param group Variable to group by (fill).
#' @param colors Custom colors. Same length as number of groups.
#' @param xlim Default is `c(0, 5)`
#' @param xlab Default is `NULL`.
#' @param nc `name_cleaner` object. Default is `RFecharts::name_cleaner()`
#' @param co `color` object. Default is `RFecharts::color()`
#' @param as_json Return as json? By default `shiny::isRunning()`, thus it will appear as chart in your console, but as json in the shiny app.
#' @param theme Commonly used RF modification for the end of the chart. Default is `RFecharts::rf_echarts_theme`.
#'
#' @return echarts4r chart
#' @export
#'
#' @examples
#' example_means |> 
#'   ec_means(x = csatorna_egyedi_atlag, group = brand, y = valasz_szovege, xlab = "uj")
#'

ec_means <- function(data, y, x, group, colors = NULL, xlim = c(0, 5), xlab = NULL, nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {

  data <- data |>
    dplyr::select(y = {{y}}, x = {{x}}, group = {{group}})

  y_axis_values <- data |> 
    dplyr::distinct(y) |> 
    nrow() |>
    magrittr::add(-1) |> 
    seq(from = 0)

  if (is.null(xlim) || xlim[1] > min(data$x, na.rm = TRUE)) {
    xlim[1] <- min(data$x, na.rm = TRUE) # maybe minus 10%
  }

  if (is.null(xlim) || xlim[2] < max(data$x, na.rm = TRUE)) {
    xlim[2] <- max(data$x, na.rm = TRUE) # maybe plus 10%
  }

  # colors
  colors_to_group <- data |> 
    dplyr::distinct(group) |> 
    tidyr::drop_na()

  if (is.null(colors)) {
    colors <- co@get(1:nrow(colors_to_group))
  }

  chart <- data |> 
    dplyr::mutate(
      y = forcats::fct_relabel(y, nc@prettify),
      y = forcats::fct_relabel(y, stringr::str_wrap, 40),
      label = nc@number(x)) |> 
    dplyr::arrange(dplyr::desc(y)) |> 
    dplyr::left_join(colors_to_group, by = dplyr::join_by(group)) |>
    dplyr::group_by(group) |>
    e_charts(x = y, reorder = FALSE) |>
    e_line(x, 
           bind = label,
           symbol = "circle",
           symbolSize = 10,
           lineStyle = list(width = 0),
           label = list(
             position = "top",
             show = TRUE,
             formatter = "{b}"
           ),
           z = 100) |>
    e_color(colors) |>
    e_y_axis(min = xlim[1], max = xlim[2], name = nc@prettify(xlab),
             nameLocation = "middle",
             nameGap = 30) |> 
    e_flip_coords() |>
    e_tooltip(trigger = "axis",
              valueFormatter =  htmlwidgets::JS("
            (value) => (value*1).toFixed(2)
    "))
  

  for (y in (y_axis_values)) {

    chart <- chart |>
      e_mark_line(data = list(yAxis = y),
                  silent = TRUE,
                  y_index = 1,
                  symbol = "none",
                  lineStyle = list(type = 'dashed',
                                   color = "#000"),
                  z = 0,
                  title = "")
  }
  
  chart |> 
    theme() |> 
    (\(x) if (as_json) echarts4r::e_inspect(x, json = T) else x) ()
}
