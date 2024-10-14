#' prep_licert
#'
#' Prepare data for licert visualization.
#'
#' @description This function prepares the license certificate data for visualization by selecting the required columns, calculating weighted means, and adding opacity and average labels.
#'
#' @param data data.frame containing the data to be prepared.
#' @param group column name or expression for the group variable.
#' @param value column name or expression for the value variable.
#' @param n column name or expression for the count variable.
#'
#' @return data.frame with prepared data
#'
#' @export
#'
#' @examples
#' example_licert |> 
#'   prep_licert(group = brand, value = value, n = n)

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
    dplyr::group_by(group) |> 
    dplyr::mutate(
      n_tech = 1,
      avg_label_pos = round(max(value) + (max(value) - min(value)) * .2)
    )
}
