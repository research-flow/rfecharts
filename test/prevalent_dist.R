boolean <- TRUE
library(tidyverse)

load("test/data.RData")

pal_jco <- RFecharts::color()@get(1:7)

add_color <- function(x){
  case_when(
    x == "prev" ~ pal_jco[1],
    x == "uj" ~ pal_jco[2],
    x == "elhagyo" ~ pal_jco[3],
    x == "elhagyo_elhagyo" ~ pal_jco[4],
    x == "elhagyo_halal" ~ "#A65628",
    x == "uj_switch" ~ pal_jco[7],
    x == "uj_uj" ~ "#FFFFB3",
  )
}

data_plot <- data |> 
  filter(ORVOS_TIPUS == first(ORVOS_TIPUS)) |>
  dplyr::mutate(IDOSZAK = as.character(IDOSZAK),
          color = add_color(ADAT_TIPUS)) |> 
  group_by(IDOSZAK, ADAT_TIPUS) |> 
  summarise(value = sum(value),
            color = first(color),
            opacity = 1) |> 
  dplyr::ungroup() |>
  dplyr::mutate(
    ADAT_TIPUS = factor(ADAT_TIPUS,
                              levels = c("prev", "uj", "elhagyo")),
          # ADAT_TIPUS = fct_relabel(ADAT_TIPUS, NiceName)
        )
  
  color_plot <- data_plot |> 
    arrange(ADAT_TIPUS) |> 
    transmute(color = add_color(fct_relabel(ADAT_TIPUS, RFecharts::name_cleaner()@prettify))) |> 
    dplyr::pull(color) |> 
    unique() |> 
    (\(x) c(NA,x))()
  
  input_data <- data_plot |> 
    bind_rows(
      data_prev |> 
        filter(ORVOS_TIPUS == first(ORVOS_TIPUS)) |>
        select(-ORVOS_TIPUS) |> 
        dplyr::mutate(ADAT_TIPUS = "prevalens")
    ) |> 
    arrange(ADAT_TIPUS) |> 
    group_by(IDOSZAK) |> 
    dplyr::mutate(
      label = ifelse(
        ADAT_TIPUS == "prevalens",
        str_c("Prevalens: ", value), ""
      ),
      prev_value = ifelse(
        ADAT_TIPUS == "prevalens",
        value, NA),
      value = ifelse(
        ADAT_TIPUS == "prevalens",
        NA, value),
      ADAT_TIPUS = ifelse(
        ADAT_TIPUS == "prevalens",
        "", ADAT_TIPUS),
    ) |> 
    dplyr::ungroup()

source("R/ec_prevalent_dist.R") 

raw_data <- input_data |> 
  transmute(
    IDOSZAK = as.numeric(IDOSZAK),
    ADAT_TIPUS = ADAT_TIPUS,
    Value = value
  )
  


example_prevalent_dist <- raw_data |>
  dplyr::mutate(ADAT_TIPUS = fct_inorder(ADAT_TIPUS))

usethis::use_data(example_prevalent_dist)

example_prevalent_dist |>  
  prep_prevalence_dist(IDOSZAK, ADAT_TIPUS, Value) |> 
  ec_prevalent_dist(nc = name_cleaner(language = "hun"))

  dplyr::mutate(type = ADAT_TIPUS, time = IDOSZAK) |> 
  ec_prevalent_dist()

  input_data |> 
    group_by(ADAT_TIPUS) |> 
    e_chart(IDOSZAK) |> 
    e_bar(prev_value, stack ="ADAT_TIPUS_2",
          name = "Prevalens",
          x_index = 1,
          bind = label,
          legend = F,
          label = list(
            show = T,
            opacity = 1,
            color = "#000",
            formatter = "{b}",
            position = "outside"
          ),
          emphasis = list(
            itemStyle = list(
              color = "#00000088"
            )
          ),
          selectedMode = FALSE,
          itemStyle =
            list(
              borderColor = "black",
              borderWidth = 1,
              shadowColor = "black",
              color = "#00000000"
            )) |>
    e_bar(value, stack = "ADAT_TIPUS",
          legend = T,
          itemStyle = list(
            shadowColor = "black"
          )
    ) |> 
    e_axis(index = 1, show = F) |> 
    e_tooltip()
