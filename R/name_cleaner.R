name_cleaner <- S7::new_class(
  name = "name_cleaner",
  properties = list(
    language = S7::class_character,
    keys = S7::class_data.frame,
    pretty_name_col = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        paste0("pretty_", self@language, "_name")
      }
    ),
    prettify = S7::new_property(getter = function(self) {
      \(x) {
        keys_chr <- dplyr::pull(self@keys, self@pretty_name_col, name = code_name)
        ifelse(x %in% names(keys_chr), keys_chr[x], x)
      }
    })
  ),
  validator = function(self) {
    stopifnot(self@pretty_name_col %in% colnames(self@keys))
    stopifnot("code_name" %in% colnames(self@keys))
  },
  constructor = function(keys, language = "hun") {
    if (missing(keys)) {
      keys <- tibble::tribble(
        ~ code_name, ~ pretty_hun_name, ~ pretty_eng_name,
        "cumn_patient", "Kumulált betegszám", "Cumulative number of patients",
        "n_patient", "Betegszám", "Number of patients",
        "patient_per_doctor", "Egy orvosra jutó betegszám", "Number of patients per doctor",
        "percentile", "Percentilis", "Percentile"
      )
    }
    S7::new_object(S7::S7_object(), language = language, keys = keys)
  }
)
