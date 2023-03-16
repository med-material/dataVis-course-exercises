library(shiny)
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("nrows",
                  "Number of rows:",
                  min = 1,
                  max = 50,
                  value = 10)
    ),
    mainPanel(
      plotOutput("carsPlot"),
      tableOutput("carsTable")
    )
  )
))