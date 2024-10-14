#' prep_delfi
#'
#' Prepare DELFI data for visualization.
#'
#' @description This function prepares the DELFI data for visualization by cleaning and formatting the data, calculating percentages, and adding border colors.
#'
#' @param data data.frame containing the data to be prepared.
#' @param value column name or expression for the value to be used for sorting and calculation.
#' @param n column name or expression for the count to be used for calculation.
#' @param consensus integer indicating the position of the consensus value. Default is -1.
#'
#' @return data.frame with prepared data
#'
#' @export
#'
#' @examples
#' example_delfi |> 
#'   prep_delfi(value = ertek_name, n = count, consensus = 1)

prep_delfi <- function(data, value, n, consensus = -1) {
  data |>
    dplyr::ungroup() |> 
    tidyr::drop_na({{ value }}, {{ n }}) |> 
    dplyr::mutate(
    dplyr::across({{ value }}, forcats::as_factor),
      percent = {{n}} / sum({{ n} }),
      borderColor = ifelse({{ value }} == dplyr::nth({{ value }}, consensus), "black", NA)
    ) |>
    dplyr::arrange(dplyr::desc({{ value }})) |> 
    dplyr::select(value = {{ value }}, percent, n = {{ n }}, borderColor, dplyr::everything())
}
