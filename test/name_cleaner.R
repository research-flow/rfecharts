library(S7)
library(tidyverse)

.pretty_names_df <- tibble::tribble(
  ~ code_name, ~ pretty_hun_name, ~ pretty_eng_name,
  "BETEG_kum", "Kumulált betegszám", "Cumulative number of patients",
  "BETEG_szazalek", "Betegszám", "Number of patients",
  "BETEG_PER_ORVOS", "Egy orvosra jutó betegszám", "Number of patients per doctor"
)

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
  constructor = function(keys, language = "hun") {
    S7::new_object("name_cleaner", language = language, keys = keys)
  }
)

prettifier <- name_cleaner(.pretty_names_df, language = "hun")

prettifier@prettify(c("BETEG_kum", "ismeretlen"))

# shiny reactive for language switch

prettifier@language <- "eng"
prettifier <- name_cleaner(.pretty_names_df, language = "eng")

prettifier@prettify(c("BETEG_kum", "ismeretlen"))




