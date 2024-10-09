match_text_color <- function(color, threshold = 0.5) {
  col <- grDevices::col2rgb(color)
  luminance <- (0.2126 * col[1,] + 0.7152 * col[2, ] + 0.0722 * col[3,]) / 255

  dplyr::case_when(
    luminance < threshold ~ "white",
    TRUE ~ "black"
  )
}
