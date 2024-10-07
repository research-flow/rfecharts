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
        "BETEG_kum", "Kumulált betegszám", "Cumulative number of patients",
        "BETEG_szazalek", "Betegszám", "Number of patients",
        "BETEG_PER_ORVOS", "Egy orvosra jutó betegszám", "Number of patients per doctor"
      )
    }
    S7::new_object(S7_object(), language = language, keys = keys)
  }
)
