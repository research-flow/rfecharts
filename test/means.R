create_05_plot <- function(data, y_axis_values = 0:4) {

  chart <- data |> 
    dplyr::mutate(
      valasz_szovege = as.character(valasz_szovege),
      valasz_szovege = fct_relabel(valasz_szovege, str_wrap, 40),
      label = scales::number(csatorna_egyedi_atlag, 
                             accuracy = 0.1L, decimal.mark = ",")) |> 
    arrange(desc(valasz_szovege)) |> 
    group_by(brand) |>
    e_charts(x = valasz_szovege, reorder = FALSE) |>
    e_line(csatorna_egyedi_atlag, 
           bind = label,
           symbol = "circle",
           symbolSize = 10,
           lineStyle = list(width = 0),
           label = list(
             position = "top",
             show = TRUE,
             formatter = "{b}"
           ),
           z = 100) |>
    e_y_axis(min = 0, max = 5, name = "Elégedettségi pontszám",
             nameLocation = "middle",
             nameGap = 30) |> 
    e_flip_coords() |>
    e_tooltip(trigger = "axis",
              valueFormatter =  htmlwidgets::JS("
            (value) => (value*1).toFixed(2)
    "))
  

  for (y in (y_axis_values)) {
    print(y)
    chart <- chart |>
      e_mark_line(data = list(yAxis = y),
                  silent = TRUE,
                  y_index = 1,
                  symbol = "none",
                  lineStyle = list(type = 'dashed',
                                   color = "#000"),
                  z = 0,
                  title = "")
  }
  
  chart
  
}

read_rds("test/barmely_barmely_masodik_abra_adat.rds") |>
  dplyr::mutate(x = csatorna_egyedi_atlag, group = brand, y = valasz_szovege) |>
  create_05_plot(y_axis_values = 0:4)

source("R/ec_means.R")
example_means <- read_rds("test/barmely_barmely_masodik_abra_adat.rds")
usethis::use_data(example_means)

ec_means(x, x = csatorna_egyedi_atlag, group = brand, y = valasz_szovege, xlim = c(3, 6), xlab = "uj", colors = RFecharts::color()@get(1:4))

