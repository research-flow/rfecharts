library(tidyverse)

test_data <- readRDS(here::here("test", "data_konc.RDS"))

example_concentration <- test_data |>
  filter(
         CSOP1 == dplyr::first(CSOP1),
         CSOP2 == dplyr::first(CSOP2),
         Szakma == dplyr::first(Szakma),
         Brand == dplyr::first(Brand),
         `Naptári év` == 2023
  ) |>
  select(`Naptári év`,
         starts_with(
           c(
             "Orvosok",
             "Össz",
             "TOP"
           )
         )) |>
  pivot_longer(cols = contains("TOP")) |>
  arrange(as.numeric(str_extract(name, "[0-9]+"))) |>
  dplyr::mutate(
    BETEG_kum = cumsum(value),
    DORVOS_kum = `Orvosok száma`/100*as.numeric(str_extract(name, "[0-9]+")),
    BETEG_PER_ORVOS = round(value/(`Orvosok száma`*10/100),0),
    # szazalek_80 = k*max(value, na.rm = T),
    name = str_extract(name, "^TOP[0-9]+"),
    BETEG_szazalek = value/sum(value, na.rm = T)
  )

usethis::use_data(example_concentration)

example_concentration |> 
  prep_concentration(`Orvosok száma`, value) |> 
  ec_concentration()

