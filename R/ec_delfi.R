#' ec_delfi
#'
#' Visualize DELFI data.
#'
#' @description This function creates an echarts4r chart to display DELFI data.
#'
#' @param data data.frame containing the data to be visualized.
#' @param nc `name_cleaner` object. Default is `RFecharts::name_cleaner()`.
#' @param colors vector of colors to use for the chart.
#' @param show_positives string indicating how to show positives. Options are "inclusive", "exclusive", or "none".
#'
#' @return echarts4r chart
#'
#' @export
#'
#' @examples
#' example_delfi |>
#'   prep_delfi(ertek_name, count) |> 
#'   ec_delfi(show_positives = "inclusive")
#' 
#' example_delfi |>
#'   prep_delfi(ertek_name, count) |> 
#'   ec_delfi(show_positives = "exclusive")
#' 
#' 
ec_delfi <- function(data, colors = NULL, show_positives = "inclusive", nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {
  if (is.null(theme)) theme <- \(x) x

  if (!is.null(colors) & length(colors) != nrow(data)) {
    cli::cli_warn("The number of colors must be equal to the number of rows in the data. Using default colors instead.")
    colors <- NULL
  }

  if (is.null(colors)) {
    if (nrow(data) == 5) {
      colors <- co@get(c(4, 1, 3, 2, 5))
    } else {
      colors <- co@get(1:nrow(data))
    }
  }

  color_to_value <- data |> 
    dplyr::ungroup() |>
    dplyr::distinct(value) |>
    dplyr::mutate(color = colors)

  data <- data |> 
    dplyr::left_join(color_to_value, by = "value")

  # select the top 50%
  if (show_positives == "exclusive") {
    positives <- data |> 
      dplyr::slice_max(value, prop = .5)

    n_positives <- nrow(positives)
    rate_positives <- sum(positives$percent)
  } else if (show_positives == "inclusive") {
    negatives <- data |> 
      dplyr::slice_min(value, prop = .5)

    n_positives <- nrow(data) - nrow(negatives)
    rate_positives <- 1 - sum(negatives$percent)
  }

  if (show_positives == "inclusive" | show_positives == "exclusive") {
    if (nc@language == "hun") { 
      positive_title = stringr::str_c("Top ", n_positives, " százaléka: ", scales::percent(rate_positives))
    } else if (nc@language == "eng") {
      positive_title = stringr::str_c("Top ", n_positives, " positive values: ", scales::percent(rate_positives))
    } else {
      positive_title = stringr::str_c("Top ", n_positives, " százaléka: ", scales::percent(rate_positives))
    }
    e_show_positives <- function(e) {
      e_mark_line(e, data = list(xAxis = 1 - rate_positives), 
                   precision= 5,
                   silent = TRUE,
                   lineStyle = list(color = "black"),
                   symbolSize = 0,
                   label = list(fontSize = 14,
                                fontWeight = "bold"),
                   title = positive_title)
    }
  } else {
    e_show_positives <- function(e) e
  }
  
  data |> 
    dplyr::mutate(
      empty_x_col = "",
      value = forcats::fct_relabel(value, nc@prettify)
    ) |> 
    dplyr::group_by(value) |> 
    e_charts(x = empty_x_col) |>
    e_bar(percent, stack = "group", bind = n, 
      label = list(show = TRUE,
        formatter = "{b}"),
        itemStdatale = list(
          shadowColor = "black",
          borderWidth = 2
      )) |> 
    e_show_positives() |> 
    e_color(colors) |> 
    e_legend(bottom = 21) |>
    e_add_nested("itemStdatale", color, borderColor) |>
    e_x_axis(axisLine = list(show=FALSE),
    axisTick = list(show=FALSE)) |>
    e_y_axis(max = 1,
    formatter = RFecharts::formatter("percent", language = nc@language)) |>
    e_title(subtext = stringr::str_c("N = ",
                  sum(data$n))
    ) |>
    e_tooltip(trigger = "axis",
    valueFormatter =
      htmlwidgets::JS("
    function(value){
    return(Number(value).toLocaleString(undefined,{stdatale: 'percent', minimumFractionDigits:2}))
    }
                    ")
    ) |>
    e_flip_coords() |>
    theme() |> 
    (\(x) if (as_json) echarts4r::e_inspect(x, json = T) else x) ()
  
}
