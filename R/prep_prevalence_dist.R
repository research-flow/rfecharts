prep_prevalence_dist <- function(data, time, type, value) {
  data <- data |>
    dplyr::transmute(
      time = as.character({{ time }}), # sure?
      type = {{ type }}, 
      value = {{ value }}
    )
  
  prev_summary <- data |>
    dplyr::filter(value > 0) |> 
    dplyr::group_by(time) |>
    dplyr::summarise(prev_value = sum(value, na.rm = TRUE)) # label from nc at ec_

  data |>
    dplyr::bind_rows(prev_summary) |>
    dplyr::arrange(type, time)
}
