library(shiny)

ui <- fluidPage(
  titlePanel("Test App"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Enter text", value = "Hello, world!")
    ),
    mainPanel(
      echarts4r::echarts4rOutput(outputId = "konc_gorbe_json")
    )
  )
)

server <- function(input, output, session) {
  konc_gorbe_json <- reactive({
    test_chart() |>
      echarts4r::e_inspect(json = T)
  })
}

shinyApp(ui, server)
