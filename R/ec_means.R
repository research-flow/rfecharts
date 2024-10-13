ec_means <- function(data, y, x, group, colors = NULL, xlim = c(0, 5), xlab = NULL, nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {

  data <- data |>
    select(y = {{y}}, x = {{x}}, group = {{group}})

  y_axis_values <- data |> 
    distinct(y) |> 
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
    distinct(group) |> 
    drop_na()

  if (is.null(colors)) {
    colors <- co@get(1:nrow(colors_to_group))
  }

  chart <- data |> 
    mutate(
      y = fct_relabel(y, str_wrap, 40),
      label = nc@number(x)) |> 
    arrange(desc(y)) |> 
    left_join(colors_to_group, by = join_by(group)) |>
    group_by(group) |>
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
  
  chart
  
}
