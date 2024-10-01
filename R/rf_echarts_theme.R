rf_echarts_theme <- function(chart) {
  chart |>
    echarts4r::e_toolbox_feature(feature = "saveAsImage") |>
    echarts4r::e_grid(
      top = 50,
      bottom = 100,
      containLabel = TRUE
    ) |>
    echarts4r::e_show_loading(
      text = "",
      color = "#8D807788"
    )
}
