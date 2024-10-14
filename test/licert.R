create_08_plot <- function(data, brand_value = "none", data_type = "ratio"){
  
  x_axis_name <- ("Promóciós értékelés")
  atlag_name <- ("Átlag")
  
  data <-
    if(brand_value == "none"){
      data } else {
        data |> 
          dplyr::mutate(csoport = 
                   case_when(
                     str_detect(brand, "HA") ~ "HA",
                     TRUE ~ "Egyeb"
                   )) |> 
          filter(csoport == brand_value)
      }
  
  
  
  data |> 
    arrange(atlag, value) |>
    dplyr::mutate(
      brand = fct_inorder(brand) ,
      value = as.character(value),
    ) |> 
    arrange(as.numeric(value)) |> 
    dplyr::mutate(value = fct_inorder(value)) |>
    left_join(brand_gyarto, by = join_by("brand_glob" == "brand")) |> 
    dplyr::mutate(opacity = format(as.hexmode(round(scales::rescale(n, to = c(20, 180)))), width = 2)) |> 
    dplyr::mutate(color = str_c(str_sub(szin, 1, 7), opacity)) |> 
    group_by(brand) |> 
    dplyr::mutate(n_tech = 1,
           n_tech_2 = 12,
           nps_lab = 
             ifelse(row_number() == 1,
                    str_c(atlag_name, ": ", scales::number(atlag, accuracy = 0.01L, decimal.mark = ",")),
                    ""),
           size = 10,
           label = scales::percent(nratio, accuracy = 0.1L, decimal.mark = ","),
           label_count = n
    ) |> 
    group_by(value) |>
    e_charts(x = brand) |>
    (\(x) if(data_type == "ratio") 
      e_bar(x, n_tech, stack = "grp", bind = label,
            label = list(
              formatter = "{b}",
              show = TRUE,
              color = "#808080df"
            ),
            silent = TRUE
      )
     else if(data_type == "count")  
       e_bar(x, n_tech, stack = "grp", bind = label_count,
             label = list(
               formatter = "{b}",
               show = TRUE,
               color = "#808080df"
             ),
             silent = TRUE
       )
    )() |>
    e_scatter(n_tech_2, symbolSize = 0,
              bind = nps_lab,
              label = list(
                formatter = "{b}",
                show = TRUE,
                color = "black",
                fontWeight = "bold"
              ),
              tooltip = list(
                # textStyle = list(
                #   color = "red"
                # ),
                formatter = "{b}"
              )
    ) |> 
    e_scatter(atlag, symbol_size = 9, itemStyle = list(color = "#86868631"),
              tooltip = list(
                # textStyle = list(
                #   color = "red"
                # ),
                formatter = htmlwidgets::JS("
      function(params){
        return(Math.round(params.value[0]*100)/100)
      }
    ")
              )) |> 
    e_add_nested("itemStyle", color) |> 
    e_y_axis(max = 12,
             maxInterval = 1, 
             axisLabel = list(
               showMaxLabel = FALSE  
             ),
             name = x_axis_name,
             nameLocation = "middle",
             nameGap = 30) |> 
    e_flip_coords() |> 
    e_legend(show = FALSE) |> 
    e_tooltip(trigger = "item")
}

brand_gyarto <- readxl::read_excel("test/gyakorisag_seged.xlsx", sheet = 2) |> 
  janitor::clean_names()


x_axis_name <- ("Promóciós értékelés")
atlag_name <- ("Átlag")

example_licert <- read_rds("test/barmely_barmely_nps_egyedi.rds") |> 
  mutate(brand = fct_anon(brand)) |> 
  select(-brand_glob)

usethis::use_data(example_licert)

example_licert |>
  prep_licert(group = brand, value = value, n = n) |> 
  ec_licert(xlab = "Promóciós értékelés", avg_color = "red", label_type = "ratio", nc = RFecharts::name_cleaner(language = "eng"))

