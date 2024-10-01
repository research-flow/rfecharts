ec_concentration <- function(data, x, n_patient, n_patient_cum, p_patient, patient_per_doctor, labels = list(), colors = character(), as_json = FALSE, theme = RFecharts::rf_echarts_theme) {
  theme <- theme %||% \(x) x

  labels$bar <- labels$bar_name %||% "Kumulált betegszám"
  labels$area <- labels$area_name %||% "Betegszám"
  labels$line <- labels$area_name %||% "Egy orvosra jutó betegszám"
  labels$x <- labels$area_name %||% "Percentilis"
  labels$y <- labels$area_name %||% "Betegszám"


  if (is.na(colors[1])) colors[1] <- "#868686FF"
  if (is.na(colors[2])) colors[2] <- "#008341FF"
  if (is.na(colors[3])) colors[3] <- "#713D2Dcc"

  data |>
    echarts4r::e_charts(x = name, reorder = F) |>
    echarts4r::e_bar(BETEG_kum,
          bind = BETEG_szazalek,
          name = "Kumulált betegszám",
          color = alpha(colors[1], 0.2),
          stack = "grp"
    ) |>
    echarts4r::e_area(
      value,
      name = str_c(NiceName("Betegszám")),
      color = pal_jco[5]
    ) |>
    echarts4r::e_line(
      BETEG_PER_ORVOS,
      name = str_c("Egy orvosra jutó ", NiceName("betegszám")),
      color = alpha(pal_jco[2], 1),
      y_index = 1
    ) |>
    echarts4r::e_x_axis(name = "Percentilis",
             nameLocation = "middle",
             nameGap = 30) |>
    echarts4r::e_y_axis(alignTicks = TRUE) |>
    echarts4r::e_y_axis(index = 1,
             name = "Orvosra jutó betegszám") |>
    echarts4r::e_y_axis(index = 0,
             name = "Betegszám") |>
    echarts4r::e_tooltip(
      trigger = "axis"
    ) |>
    theme() |>
    (\(x) if (as_json) echarts4r::e_inspect(x, json = T) else x) ()
}
