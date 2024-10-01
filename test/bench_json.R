bench::mark(
  pure = {
    test_chart()
  },
  as_json = {
    test_chart() |>
      echarts4r::e_inspect(json = T)
  }, check = FALSE
)

object.size(test_chart())
object.size(test_chart() |> echarts4r::e_inspect(json = T)) # 10-ede a küldendő adat

