ec_concentration <- function(data, n_patient, n_doctor, nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {
  theme <- theme %||% \(x) x

  data <- data |> 
    select(n_doctor = {{ n_doctor }}, n_patient = {{ n_patient }}) |>
    mutate(patient_per_doctor = n_patient / n_doctor) |> 
    arrange(n_patient) |>
    mutate(
      patient_per_doctor = round(patient_per_doctor, 0),
      percent_patient = n_patient / sum(n_patient),
      cumn_patient = cumsum(n_patient),
      percentile_category = rev(str_c("TOP", round((row_number() / n()) * 100))),
    ) |> 
    arrange(desc(n_patient))

  data |> 
    echarts4r::e_charts(x = percentile_category, reorder = F) |>
    echarts4r::e_bar(cumn_patient,
          bind = percent_patient,
          name = nc@prettify("cumn_patient"),
          color = co@get(1, .2),
          stack = "grp"
    ) |>
    echarts4r::e_area(
      n_patient,
      name = nc@prettify("n_patient"),
      color = co@get(5)
    ) |>
    echarts4r::e_line(
      patient_per_doctor,
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
    echarts4r::e_y_axis(index = 0, formatter = RFecharts::formatter(language = nc@language),
           name = nc@prettify("n_patient")) |>
    echarts4r::e_tooltip(
      trigger = "axis"
    ) |>
    theme() |>
    (\(x) if (as_json) echarts4r::e_inspect(x, json = T) else x) ()
}
