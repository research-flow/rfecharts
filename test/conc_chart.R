library(tidyverse)

test_data <- readRDS(here::here("test", "data_konc.RDS"))

pal_jco <-
  c("#0073C2FF",
    "#713D2Dcc",
    "#868686FF",
    "#CD534CFF",
    "#008341FF",
    "#7AA6DCFF",
    "#003C67FF",
    "#C89B7Cff",
    "#EFC000FF",
    "#D07C2FFF",
    "#8D8077FF",
    "#A73030FF"
  )

NiceName <- \(x) x


test_data |>
  filter(
         CSOP1 == first(CSOP1),
         CSOP2 == first(CSOP2),
         Szakma == first(Szakma),
         Brand == first(Brand),
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
  mutate(
    BETEG_kum = cumsum(value),
    DORVOS_kum = `Orvosok száma`/100*as.numeric(str_extract(name, "[0-9]+")),
    BETEG_PER_ORVOS = round(value/(`Orvosok száma`*10/100),0),
    # szazalek_80 = k*max(value, na.rm = T),
    name = str_extract(name, "^TOP[0-9]+"),
    BETEG_szazalek = value/sum(value, na.rm = T)
  ) |>
  ec_concentration(x = name, n_patient = BETEG_kum, n_patient_cum = BETEG_kum, p_patient = BETEG_szazalek, patient_per_doctor = BETEG_PER_ORVOS)
