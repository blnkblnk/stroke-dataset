library(shiny)
x <- read.csv("./healthcare-dataset-stroke-data.csv")
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Statistics",
      p(
        "test"
      ),
    ),
    tabPanel(
      "Visualization",
      p(
        "test"
      ),
    ),
    tabPanel(
      "Statistical Analysis ",
      p(
        "test"
      ),
    ),
  )
)
server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)