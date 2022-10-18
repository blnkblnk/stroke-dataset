library(shiny)
x <- read.csv("healthcare-dataset-stroke-data.csv")
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Statistics",
      p(
        "Filter | Log file, Track how many are left"
      ),
    ),
    tabPanel(
      "Visualization",
      p(
        "Generate plots depending on variables selected"
      ),
    ),
    tabPanel(
      "Statistical Analysis",
      p(
        "Prediction : Logistical Model"
      ),
    ),
  )
)
server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)