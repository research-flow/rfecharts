co <- RFecharts::color()

co@get(1, alpha =.2)
co@get(1:10)
co@get(c("high", "green"))

# modify the palette
co@names_for_hex <- co@names_for_hex |> 
  dplyr::add_row(color_name = "new_color", hex = "#824bac")

co@get("new_color")
