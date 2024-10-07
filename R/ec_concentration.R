ec_concentration <- function(data, x, n_patient, cumn_patient, p_patient, patient_per_doctor, nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {
  theme <- theme %||% \(x) x

  data |>
    echarts4r::e_charts(x = name, reorder = F) |>
    echarts4r::e_bar(BETEG_kum,
          bind = BETEG_szazalek,
          name = nc@prettify("cumn_patient"),
          color = co@get(1, .2),
          stack = "grp"
    ) |>
    echarts4r::e_area(
      value,
      name = nc@prettify("n_patient"),
      color = co@get(5)
    ) |>
    echarts4r::e_line(
      BETEG_PER_ORVOS,
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
    echarts4r::e_y_axis(index = 0, formatter = RFecharts::formatter(),
           name = nc@prettify("n_patient")) |>
    echarts4r::e_tooltip(
      trigger = "axis"
    ) |>
    theme() |>
    (\(x) if (as_json) echarts4r::e_inspect(x, json = T) else x) ()
}
