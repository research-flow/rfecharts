prep_delfi <- function(data, value, n, consensus = -1) {
  data |>
    dplyr::ungroup() |> 
    dplyr::drop_na({{ value }}, {{ n }}) |> 
    dplyr::mutate(
      dplyr::across({{ value }}, as_factor),
      percent = {{n}} / sum({{ n} }),
      borderColor = ifelse({{ value }} == dplyr::nth({{ value }}, consensus), "black", NA)
    ) |>
    dplyr::arrange(dplyr::desc({{ value }})) |> 
    dplyr::select(value = {{ value }}, percent, n = {{ n }}, borderColor, dplyr::everything())
}