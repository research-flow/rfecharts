prep_licert <- function(data, group, value, n) {
  data <- data |>
    dplyr::select(group = {{ group }}, value = {{ value }}, n = {{ n }})
  
  means <- data |>
    dplyr::ungroup() |>
    dplyr::group_by(group) |>
    dplyr::summarise(avg = weighted.mean(value, w = n, na.rm = TRUE))

  data |> 
    dplyr::left_join(means, by = dplyr::join_by(group)) |> 
    dplyr::mutate(
      opacity = format(as.hexmode(round(scales::rescale(n, to = c(20, 180)))), width = 2)
    ) |> 
    group_by(group) |> 
    mutate(
      n_tech = 1,
      n_tech_2 = 12,
      size = 10
    )
}
