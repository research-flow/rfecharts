library(tidyverse)
library(echarts4r)

data <- read_rds("test/df_nested_output.rds")

common_echarts_utils <- function(echarts) {
  echarts |>
    e_grid(
      top = 50,
      bottom = 100,
      containLabel = TRUE
    ) |>
    e_show_loading(
      text = "",
      color = "#8D807788"
    )
}

color_table <-
  tibble(
    ertek = seq(1,5),
    color = ggsci::pal_jco()(5)[order(c(5,2,3,1,4))]
  )

delfi_abra <- function(data){
  data |>
    mutate(
      x = "",

      borderColor =
        ifelse(ertek == konszenzus_value,
               "black",
               NA)
    ) |>
    arrange(desc(ertek)) |>
    mutate(ertek_name = forcats::fct_inorder(ertek_name)) |>
    group_by(ertek_name) |>
    (\(y) e_charts(y, x = x) |>
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
                   title = str_c("Top 3 százaléka: ", scales::percent(first(y$top3_percent)))) |>
       e_color(color) |>
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
    )()
}

delfi_abra(color_table)
