#' prep_prevalence_dist
#'
#' Prepare prevalent distribution data for visualization.
#'
#' @description This function prepares the prevalent distribution data for visualization by selecting the required columns, filtering out zero values, calculating summary statistics, and binding the results together.
#'
#' @param data data.frame containing the data to be prepared.
#' @param time column name or expression for the time variable.
#' @param type column name or expression for the type variable.
#' @param value column name or expression for the value variable.
#'
#' @return data.frame with prepared data
#'
#' @export
#'
#' @examples
#' example_prevalent_dist |>  
#'   prep_prevalence_dist(IDOSZAK, ADAT_TIPUS, Value)
#' 

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
