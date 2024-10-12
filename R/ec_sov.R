ec_sov <- function(data, x_label = NULL, colors = NULL, nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {

  theme <- theme %||% \(x) x

  # check type for percent
  fill_type <- data |> 
    group_by(y) |>
    summarise(close_to_one = abs(sum(x) - 1) < .01) |> 
    print() |> 
    pull(close_to_one) |> 
    all(na.rm = TRUE)

  if (fill_type) {
    x_formatter <- RFecharts::formatter(style = "percent", language = nc@language)
  } else {
    x_formatter <- RFecharts::formatter(language = nc@language)
  }

  # colors
  colors_to_group <- data |> 
    distinct(group) |> 
    drop_na()

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

  if (is.null(colors)) {
    colors <- co@get(1:nrow(colors_to_group))
  }

  colors_to_group <- colors_to_group |>
    mutate(color = colors)

  if (fill_type) {
    data <- data |> 
      mutate(label = nc@percent(x))
  } else {
    data <- data |> 
      mutate(label = nc@number(x))
  }

  data |> 
    print() |> 
    left_join(colors_to_group, by = join_by(group)) |>
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
    theme()
}
