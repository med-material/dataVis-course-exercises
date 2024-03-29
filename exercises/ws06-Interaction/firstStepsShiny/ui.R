library(shiny)
fluidPage(
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      numericInput("numInput", "A numeric input:", value = 7, min = 1, max = 30)
    ),
    mainPanel(
      textOutput("txtOutput"),
      p(strong("bold font "), em("italic font")),
      p(code("code block")),
      a(href="http://www.google.com", "link to Google"))
  )
)