color <- S7::new_class(
  name = "color",
  properties = list(
    names_for_hex = S7::class_data.frame,
    codes_for_names = S7::class_data.frame,
    get = S7::new_property(getter = function(self) {
      \(x, alpha = 1) {
        if (is.numeric(x)) {
          if (max(x) > nrow(self@names_for_hex)) {
            cli::cli_alert_warning("You are trying to access a color that is not
                                   in the color palette.")
            # TODO solution for this
          }
          dplyr::pull(self@names_for_hex, hex)[x]
        } else {
          # convert `code_name` to `color_name`
          codes_for_names_chr <-
            dplyr::pull(self@codes_for_names, color_name, name = code_name)

          x <- ifelse(x %in% names(codes_for_names_chr), codes_for_names_chr[x], x)

          # convert `color_name` to `hex`
          names_for_hex_chr <-
            dplyr::pull(self@names_for_hex, hex, name = color_name)

          ifelse(x %in% names(names_for_hex_chr), names_for_hex_chr[x], x) |>
            ggplot2::alpha(alpha = alpha)
        }
      }
    })
  ),
  validator = function(self) {
    stopifnot("color_name" %in% colnames(self@names_for_hex))
    stopifnot("hex" %in% colnames(self@names_for_hex))
    stopifnot("code_name" %in% colnames(self@codes_for_names))
    stopifnot("color_name" %in% colnames(self@codes_for_names))
  },
  constructor = function(names_for_hex, codes_for_names) {
    if (missing(names_for_hex)) {
      names_for_hex <- tibble::tribble(
        ~ color_name, ~ hex,
        # pal_cols
        "blue", "#0073C2FF",
        "brown", "#713D2Dcc",
        "grey", "#868686FF",
        "red", "#CD534CFF",
        "green", "#008341FF",
        "blue2", "#7AA6DCFF",
        "blue3", "#003C67FF",
        "brown2", "#C89B7Cff",
        "yellow", "#EFC000FF",
        "orange", "#D07C2FFF",
        "grey2", "#8D8077FF",
        "red2", "#A73030FF",
        # brewer.par 8
        "cyan", "#66C2A5",
        "orange2", "#FC8D62",
        "purple", "#8DA0CB",
        "magenta", "#E78AC3",
        "green2", "#A6D854",
        "yellow2", "#FFD92F",
        "brown3", "#E5C494",
        "grey3", "#B3B3B3",
        # viridis
        "viridis1", "#440154FF",
        "viridis2", "#482878FF",
        "viridis3", "#3E4A89FF",
        "viridis4", "#31688EFF",
        "viridis5", "#26828EFF",
        "viridis6", "#1F9E89FF",
        "viridis7", "#35B779FF",
        "viridis8", "#6DCD59FF",
        "viridis9", "#B4DE2CFF",
        "viridis10", "#FDE725FF"
        )

    }
    if (missing(codes_for_names)) {
      codes_for_names <- tibble::tribble(
        ~ code_name, ~ color_name,
        "high", "#7AA6DCFF",
        "low","#A73030FF",
        "bg", "#ffffff",
        "fg", "#000000",
        "primary", "#713D2DBF",
        "secondary", "#8D8077BF",
        "success", "#008341FF",
        "warning", "#C89B7Cff",
        "danger", "#A73030FF",
        "info", "#D07C2FFF"
      )
    }
    S7::new_object(S7::S7_object(), names_for_hex = names_for_hex, codes_for_names = codes_for_names)
  }
)
