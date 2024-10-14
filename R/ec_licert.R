#' ec_licert
#'
#' Visualize license certificate data.
#'
#' @description This function creates an echarts4r chart to display license certificate data.
#'
#' @param data data.frame containing the data to be visualized.
#' @param label_type string indicating the type of label to use. Options are "ratio" or "number". Default is "ratio".
#' @param xlab string indicating the x-axis label. Default is NULL.
#' @param colors vector of colors to use for the chart. Default is NULL.
#' @param avg_color string indicating the color to use for the average line. Default is "#86868631".
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
#' example_licert |>
#'   prep_licert(group = brand, value = value, n = n) |> 
#'   ec_licert(xlab = "Rating", avg_color = "red", label_type = "ratio", nc = RFecharts::name_cleaner(language = "eng"))
#' 
#' example_licert |>
#'   prep_licert(group = brand, value = value, n = n) |> 
#'   ec_licert(xlab = "Promóciós értékelés", label_type = "count")
#' 

ec_licert <- function(data, label_type = "ratio", xlab = NULL, colors = NULL, avg_color = "#86868631", nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {
    
  # label
  if (label_type == "ratio") { 
    data <- data |> 
      group_by(group) |>
      dplyr::mutate(label = nc@percent(n / sum(n)))
  } else {
    data <- data |> 
      group_by(group) |>
      dplyr::mutate(label = nc@number(n))
  }
    
  if (nc@language == "hun") {
    data <- data |>
      dplyr::mutate(avg_label = paste("Átlag:", nc@number(avg)))
  } else {
    data <- data |>
      dplyr::mutate(avg_label = paste("Avg:", nc@number(avg)))
  }

  # colors
  if (is.null(colors)) {
    colors <- co@get(1:dplyr::n_distinct(data$group))
  }

  color_to_group <- data |> 
    dplyr::ungroup() |>
    dplyr::distinct(group) |>
    dplyr::mutate(color = colors)
    
  data <- data |>
    dplyr::ungroup() |> 
    dplyr::left_join(color_to_group, by = dplyr::join_by(group)) |>
    dplyr::mutate(
      avg_label_pos = round(max(value) + (max(value) - min(value)) * .2),
      color = stringr::str_c(stringr::str_sub(color, 1, 7), opacity)
    )
    
  data |> 
    dplyr::mutate(ntech = 1) |> 
    dplyr::group_by(value) |>
    e_charts(x = group) |>
    e_bar(n_tech, stack = "grp", bind = label, label = list(
      formatter = "{b}",
      show = TRUE,
      color = "#808080df"
    ), silent = TRUE) |> 
      e_scatter(avg_label_pos, symbolSize = 0,
                bind = avg_label,
                label = list(
                  formatter = "{b}",
                  show = TRUE,
                  color = "black",
                  fontWeight = "bold"
                ),
                tooltip = list(show = FALSE)
      ) |> 
      e_scatter(avg, symbol_size = 9, itemStyle = list(color = avg_color),
                tooltip = list(formatter = "{b}")) |>
      e_add_nested("itemStyle", color) |> 
      e_y_axis(max = dplyr::first(data$avg_label_pos),
              maxInterval = 1, 
              axisLabel = list(
                showMaxLabel = FALSE  
              ),
              name = nc@prettify(xlab),
              nameLocation = "middle",
              nameGap = 30) |> 
      e_flip_coords() |> 
      e_legend(show = FALSE) |> 
      e_tooltip(trigger = "item") |> 
      theme() |> 
      (\(x) if (as_json) echarts4r::e_inspect(x, json = T) else x) ()
}
