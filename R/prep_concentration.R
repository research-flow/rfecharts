#' prep_concentration
#'
#' Prepare data for concentration visualization.
#'
#' @description This function prepares the data for visualization by calculating patient per doctor, percent patient, and cumulative patient values.
#'
#' @param data data.frame containing the data to be prepared.
#' @param n_doctor column name or expression for the number of doctors.
#' @param n_patient column name or expression for the number of patients.
#'
#' @return data.frame with prepared data
#'
#' @export
#'
#' @examples
#' example_concentration |> 
#'   prep_concentration(n_doctor = `Orvosok száma`, n_patient = value)
#' 

prep_concentration <- function(data, n_doctor, n_patient) {
  data |> 
    dplyr::select(n_doctor = {{ n_doctor }}, n_patient = {{ n_patient }}) |>
    dplyr::mutate(patient_per_doctor = n_patient / n_doctor) |> 
    dplyr::arrange(n_patient) |>
    dplyr::mutate(
      patient_per_doctor = round(patient_per_doctor, 0),
      percent_patient = n_patient / sum(n_patient, na.rm = TRUE),
      cumn_patient = cumsum(n_patient),
      percentile_category = rev(stringr::str_c("TOP", round((dplyr::row_number() / dplyr::n()) * 100))),
    ) |> 
      dplyr::arrange(dplyr::desc(n_patient))
  }