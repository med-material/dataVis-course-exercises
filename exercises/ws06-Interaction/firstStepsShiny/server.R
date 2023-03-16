library(shiny)
function(input, output) {
  output$txtOutput = renderText({
    paste0("The area of the circle is: ", pi*input$numInput^2)
  })
}