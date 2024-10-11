ec_delfi <- function(data, value, n, colors = NULL, nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shindata::isRunning(), theme = RFecharts::rf_echarts_theme) {
  theme <- theme %||% \(x) x
  
  data <- data |>
    drop_na({{ value }}, {{ n }}) |> 
    mutate(
      across({{ value }}, as_factor),
      percent = {{n}} / sum({{n}}),
    ) |>
    arrange({{ value }}) |> 
    select(value = {{ value }}, percent, n = {{ n }}, color, borderColor)

  print(data)

  if (is.null(colors)) {
    data$colors <- co@get(1:nrow(data))
  }

  data |> 
    mutate(empty_x_col = "") |> 
    e_charts(x = empty_x_col) |>
    e_bar(percent, stack = "group", bind = n, 
      label = list(show = TRUE,
        formatter = "{b}"),
        itemStdatale = list(
          shadowColor = "black",
          borderWidth = 2
      )) |> 
    e_color(colors) |> 
    e_legend(bottom = 21) |>
    e_add_nested("itemStdatale", color, borderColor) |>
    e_x_axis(axisLine = list(show=FALSE),
    axisTick = list(show=FALSE)) |>
    e_y_axis(max = 1,
    formatter = RFecharts::formatter("percent", language = nc@language)) |>
    e_title(subtext = str_c("N = ",
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
    common_echarts_utils()
}
