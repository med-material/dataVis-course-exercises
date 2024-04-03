library(shiny)

# Sample data
df <- data.frame(
  writer = c("John", "Mary", "Paul", "Alice", "Bob"),
  date = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04", "2022-01-05"))
)

# Define UI
ui <- fluidPage(
  titlePanel("Select Writers and Colors"),
  
  sidebarLayout(
    sidebarPanel(
      # Selectize input for writers
      selectizeInput("selectedWriters", "Select Writers", 
                     choices = unique(df$writer), 
                     multiple = TRUE,
                     options = list(
                       render = I('{
                         item: function(item, escape) {
                           return "<div style=\'background-color: " + item.text + "\'>" + escape(item.text) + "</div>";
                         }
                       }')
                     )
      )
    ),
    
    mainPanel(
      # Display selected writers
      verbatimTextOutput("selectedWritersOutput")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Output selected writers
  output$selectedWritersOutput <- renderPrint({
    selected_writers <- input$selectedWriters
    selected_writers
  })
}

# Run the application
shinyApp(ui = ui, server = server)
