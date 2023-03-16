library(shiny)
shinyServer(function(input, output) {
  output$carsPlot <- renderPlot({
    plot(head(cars, input$nrows))
  })
  output$carsTable <- renderTable({
    head(cars, input$nrows)
  })
})