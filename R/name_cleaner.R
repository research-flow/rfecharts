#' name_cleaner Class
#'
#' The name cleaner class provides a way to clean and format names for use in echarts4r charts.
#'
#' @description This class provides a way to work with names in your application, including formatting and translating names.
#'
#' @section Methods:
#'   \describe{
#'     \item{\code{prettify}}{Format a name for display.}
#'     \item{\code{percent}}{Format a percentage value for display.}
#'     \item{\code{number}}{Format a number value for display.}
#'   }
#'
#' @section Properties:
#'   \describe{
#'     \item{\code{language}}{The language to use for formatting and translation.}
#'     \item{\code{keys}}{A data frame containing the mapping of code names to pretty names.}
#'     \item{\code{pretty_name_col}}{The column name in the keys data frame that contains the pretty names.}
#'     \item{\code{dec.mark}}{The decimal mark to use for formatting numbers.}
#'     \item{\code{big.mark}}{The big mark to use for formatting numbers.}
#'   }
#'
#' @param keys data.frame containing the mapping of code names to pretty names. Default is a built-in data frame.
#' @param language The language to use for formatting and translation. Default is "hun".
#'
#' @return name_cleaner object
#'
#' @export
#'
#' @examples
#' nc <- RFecharts::name_cleaner(language = "eng")
#' nc@prettify("cumn_patient")
#' nc@pnumber(500123.23)
#' 
#' nc <- RFecharts::name_cleaner(language = "hun")
#' nc@prettify("cumn_patient")
#' nc@pnumber(500123.23)
#' 

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
    }),
    dec.mark = S7::new_property(getter = function(self) {
      if (self@language == "hun") {
        ","
      } else {
        "."
      }
    }),
    big.mark = S7::new_property(getter = function(self) {
      if (self@language == "hun") {
        " "
      } else {
        ","
      }
    }),
    percent = S7::new_property(getter = function(self) {
      \(x, accuracy = 1, ...) scales::percent(x, accuracy = accuracy, decimal.mark = self@dec.mark, big.mark = self@big.mark, ...)
    }),
    number = S7::new_property(getter = function(self) {
      \(x, accuracy = NULL, ...) {
        if (is.null(accuracy)) accuracy <- ifelse(all(x %% 1 == 0, na.rm = TRUE), 1, .01) # integers?
        scales::number(x, accuracy = accuracy, decimal.mark = self@dec.mark, big.mark = self@big.mark, ...)
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
        "percentile", "Percentilis", "Percentile",
        "prev", "Folytató beteg", "Prevalence",
        "uj", "Incidens beteg", "Incidence",
        "elhagyo", "Terápiaelhagyás miatt nem megvalósult \n(előző időszak elhagyó)", "Lost to follow-up due to illness",
        "elhagyo_elhagyo", "Elhagyó beteg", "Lost to follow-up",
        "elhagyo_halal", "Halálozás", "Death",
        "uj_switch", "Incidens: változott", "Incidence: switch",
        "uj_uj", "Incidens: új", "Incidence: new"
      )
    }
    S7::new_object(S7::S7_object(), language = language, keys = keys)
  }
)
