test_chart <- function() {
  iris |>
    dplyr::group_by(Species) |>
    echarts4r::e_charts(Sepal.Length) |>
    echarts4r::e_line(Sepal.Width) |>
    echarts4r::e_lm(Sepal.Width ~ Sepal.Length) |>
    echarts4r::e_x_axis(min = 4)
}

