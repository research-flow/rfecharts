prep_concentration <- function(data, n_doctor, n_patient) {
  data <- data |> 
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