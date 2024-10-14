board_value_val <- pins::board_folder("test")
i <- 1
orvos_tipus_total_val <- "barmely"
brand <- "Beba"
brand_gyarto <- readxl::read_excel("test/gyakorisag_seged.xlsx", sheet = 2) |> 
  janitor::clean_names()


create_plot_data_1 <- function(data_egyedi, data_total, lang = lang_glob){
  data_egyedi |> 
    left_join(select(data_total, - any_of("n_orvos"))) |> 
    dplyr::mutate(
      csatorna_arany = csatorna_egyedi / csatorna_osszes,
      csatorna_per_orvos = csatorna_egyedi / n_orvos,
      label = scales::percent(csatorna_arany, accuracy = 0.1L, decimal.mark = ",")
    ) |> 
    left_join(
      brand_gyarto
    ) |>
    dplyr::mutate(brand = fct_inorder(brand),
           valasz_szovege = fct_relabel(valasz_szovege, str_wrap, 40)
    ) |>
    rename(color = szin)
}

create_plot_data_2 <- function(data_egyedi, data_total){
  data_total |> 
    select(-n_orvos) |> 
    bind_cols(data_egyedi) |> 
    dplyr::mutate(
      csatorna_arany = csatorna_egyedi / csatorna_osszes,
      csatorna_per_orvos = csatorna_egyedi/n_orvos,
      label = scales::percent(csatorna_arany, accuracy = 0.1L, decimal.mark = ",")
    ) |> 
    left_join(
      brand_gyarto
    ) |>
    dplyr::mutate(brand = fct_inorder(brand),
           valasz_szovege = 
             
               " Összes csatorna"
              
    ) |>
    rename(color = szin)
}

data_type = "ratio"

data_1 = create_plot_data_1(
  read_rds("test/barmely_barmely_csatorna_sov_egyedi.rds"),
  read_rds("test/barmely_barmely_csatorna_sov_total.rds")
)
  
data_2 = create_plot_data_2(
  read_rds("test/barmely_barmely_sov_egyedi.rds"),
  read_rds("test/barmely_barmely_sov_total.rds")
)

example_sov <- bind_rows(data_1,data_2) |> 
  arrange(desc(valasz_szovege)) |> 
  dplyr::mutate(csatorna_per_orvos = csatorna_per_orvos) |> 
  dplyr::mutate(csatorna_egyedi = round(csatorna_egyedi, 0)) |> 
  mutate(brand = fct_anon(brand))

usethis::use_data(example_sov)

example_sov |> 
  ec_sov(x = csatorna_egyedi, y = valasz_szovege, group = brand, nc = name_cleaner(language = "hun"))

bind_rows(data_1,data_2) |> 
  arrange(desc(valasz_szovege)) |> 
  dplyr::mutate(csatorna_per_orvos = scales::number(csatorna_per_orvos, accuracy = 0.01L)) |> 
  group_by(brand) |> 
  e_charts(x = valasz_szovege, reorder = F) |>
  e_bar(csatorna_arany, stack = "grp", bind = label,
    label = list(
    position = "inside",
    show = TRUE,
    formatter = "{b}"
  )) |>
  e_flip_coords() |>
  e_tooltip(
    trigger = "axis",
    valueFormatter =  htmlwidgets::JS("
          (value) => (value*100).toFixed(1) + '%'
    ")) |> 
  e_x_axis(
    formatter = e_axis_formatter(style = "percent", digits = 1),
    max = 1, name = NULL, # TODO
    nameLocation = "middle",
    nameGap = 30
  )
    

  create_03_plot <- function(data_1, data_2, data_type = "ratio"){
    bind_rows(
      data_1,
      data_2) |> 
      arrange(desc(valasz_szovege)) |> 
      dplyr::mutate(csatorna_per_orvos = scales::number(csatorna_per_orvos, accuracy = 0.01L)) |> 
      group_by(brand) |> 
      e_charts(x = valasz_szovege, reorder = F) |>
      (\(x) if(data_type == "ratio") 
        e_bar(x, csatorna_arany, stack = "grp", bind = label,
              label = list(
                position = "inside",
                show = TRUE,
                formatter = "{b}"
              )) |>
         e_flip_coords() |>
         e_tooltip(trigger = "axis",
                   valueFormatter =  htmlwidgets::JS("
              (value) => (value*100).toFixed(1) + '%'
      ")) |> 
         e_x_axis(formatter = e_axis_formatter(style = "percent", digits = 1),
                  max = 1, name = i18n$t("SOV arány"),
                  nameLocation = "middle",
                  nameGap = 30)
       else if(data_type == "count")  
         e_bar(x, csatorna_egyedi, stack = "grp", bind = csatorna_egyedi,
               label = list(
                 position = "inside",
                 show = TRUE,
                 formatter = "{b}"
               )) |>
         e_tooltip(trigger = "axis") |>
         e_flip_coords() |>
         e_x_axis(name = i18n$t("SOV érték"),
                  nameLocation = "middle",
                  nameGap = 30)
       else if(data_type == "perorvos")  
         e_bar(x, csatorna_per_orvos, stack = "grp", bind = csatorna_per_orvos,
               label = list(
                 position = "inside",
                 show = TRUE,
                 formatter = "{b}"
               )) |>
         e_tooltip(trigger = "axis") |>
         e_flip_coords() |>
         e_x_axis(name = i18n$t("SOV érték per orvos"),
                  nameLocation = "middle",
                  nameGap = 30)
      )() |>
      e_color(unname(gp_colors[levels(data_1$brand)])) |>
      common_echarts_utils()
  }
  
  