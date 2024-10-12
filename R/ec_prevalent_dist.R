ec_prevalent_dist <- function(data, colors = NULL,nc = RFecharts::name_cleaner(), co = RFecharts::color(), as_json = shiny::isRunning(), theme = RFecharts::rf_echarts_theme) {
  theme <- theme %||% \(x) x

  if (nc@language == "eng") {
    prev_label <- "Prevalent"
  } else { # default to HUN
    prev_label <- "Prevalens"
  }
  
  color_to_types <- data |>  # FIXME
    dplyr::distinct(type) |> 
    dplyr::filter(!is.na(type))

  if (is.null(colors)) {
    if (nrow(color_to_types) == 3) { # add more later
      colors <- co@get(c(2, 1, 3))
    } else {
      colors <- co@get(1:nrow(color_to_types))
    }
  }

  color_to_types <- color_to_types |>
    dplyr::mutate(color = colors)


  data |> 
    dplyr::mutate(
      label = dplyr::case_when(
        is.na(prev_label) ~ NA_character_,
        TRUE ~ paste0(prev_label, ": ", prev_value),
      ),
      opacity = ifelse(is.na(prev_value), NA, 1)
    ) |> 
    dplyr::left_join(color_to_types, by = dplyr::join_by(type)) |>
    dplyr::group_by(type) |> 
    tidyr::replace_na(list(type = "", label = "")) |>
    dplyr::mutate(type = fct_relabel(type, nc@prettify)) |>
    e_chart(time) |> 
    e_bar(prev_value, stack ="type_2",
          name = "Prevalens",
          x_index = 1,
          bind = label,
          legend = F,
          label = list(
            show = T,
            opacity = 1,
            color = "#000",
            formatter = "{b}",
            position = "outside"
          ),
          emphasis = list(
            itemStyle = list(
              color = "#00000088"
            )
          ),
          selectedMode = FALSE,
          itemStyle =
            list(
              borderColor = "black",
              borderWidth = 1,
              shadowColor = "black",
              color = "#00000000"
            )) |>
    e_bar(value, stack = "type",
          legend = T,
          itemStyle = list(
            shadowColor = "black"
          )
    ) |> 
    e_axis(index = 1, show = F) |> 
    e_tooltip() |> 
    theme()
}
