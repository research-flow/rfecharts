library(tidyverse)
library(echarts4r)

data <- read_rds("test/df_nested_output.rds")


example_delfi <- data |> 
  filter(name == first(name))

usethis::use_data(example_delfi)

example_delfi |>
  prep_delfi(ertek_name, count) |> 
  ec_delfi(show_positives = "inclusive")

example_delfi |>
  prep_delfi(ertek_name, count) |> 
  ec_delfi(show_positives = "exclusive")


y <- questions_raw |>
  filter(name == first(name)) |> 
      dplyr::mutate(
        x = "",

        borderColor =
          ifelse(ertek == konszenzus_value,
                 "black",
                 NA)
      ) |>
      arrange(desc(ertek)) |>
      dplyr::mutate(ertek_name = forcats::fct_inorder(ertek_name))

source("R/ec_delfi.R")
ec_delfi(y, ertek_name, count)

e_charts(y, x = x) |> 
e_bar(percent, stack = "group", bind = count,
             label = list(show = TRUE,
                          formatter = "{b}"),
             itemStyle = list(
               shadowColor = "black",
               borderWidth = 2
             )) |> 
       e_mark_line(data = list(xAxis = first(y$top3_percent)), 
                   precision= 5,
                   silent = TRUE,
                   lineStyle = list(color = "black"),
                   symbolSize = 0,
                   label = list(fontSize = 14,
                                fontWeight = "bold"),
                   title = stringr::str_c("Top 3 százaléka: ", scales::percent(first(y$top3_percent)))) |> 
       e_color(y$color) |> 
       e_legend(bottom = 21) |>
       e_add_nested("itemStyle", color, borderColor) |> 
       e_x_axis(axisLine = list(show=FALSE),
                axisTick = list(show=FALSE)) |>
       e_y_axis(max = 1,
                formatter = e_axis_formatter("percent")) |> 
       e_title(subtext = str_c("N = ", 
                               sum(y$count))
       ) |>
       e_tooltip(trigger = "axis",
                 valueFormatter = 
                   htmlwidgets::JS("
                function(value){
                return(Number(value).toLocaleString(undefined,{style: 'percent', minimumFractionDigits:2}))
                }
                                 ")
       ) |> 
       e_flip_coords() |>
       common_echarts_utils()
