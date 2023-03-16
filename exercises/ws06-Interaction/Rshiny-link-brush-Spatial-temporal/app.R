## Install and load needed libraries
#install.packages("plotly")
#install.packages("dplyr")
#install.packages("shiny")
library(shiny)
#install.packages("htmlwidgets")
library(htmlwidgets)
library(tidyverse)
library(plyr)
library(lubridate)
library(plotly)
library(dplyr)
library(here)

# Suppport for Milliseconds
options("digits.secs"=6)

timetemplate <- plot_ly() %>%
    config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover")) %>%
    layout(dragmode = "pan", showlegend=FALSE, xaxis = list(tickformat="%H:%M:%S.%L ms"), yaxis = list(range=c(0,1.1)))

vistemplate <- plot_ly() %>%
    config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover")) %>%
    layout(dragmode = "pan", showlegend=FALSE)

## Read the fake csv data set about ice cream
D_event<-read.csv(here("evalLog_2020-05-13 15-38-26.2363.csv"), na.string="NULL")
D_event <- D_event %>% mutate(TimeStamp = as.POSIXct(TimeStamp, format = "%Y-%m-%d %H:%M:%OS"))
D_event$rowID <- 1:nrow(D_event)

# Add label positions
D_event <- D_event %>%
    mutate(labelpos = 0.5,
           labelpos = ifelse(D_event$EventType == "MoleEvent", 0.2, labelpos),
           labelpos = ifelse(D_event$EventType == "PointerEvent", 0.3, labelpos),
           labelpos = ifelse(D_event$EventType == "ModifierEvent", 0.4, labelpos))

Wall_moles <- expand.grid(0:D_event$WallColumnCount[1], 0:D_event$WallRowCount[1]) %>%
    rename(x = Var1, y = Var2)

## Create the shiny UI layout
ui <- fluidPage(
    headerPanel("Whack-A-Mole VR"),
    mainPanel(
        plotlyOutput("TimePlot"),
        h4("Use the box select to learn more about which mole belongs the given timeline."),
        plotlyOutput("WhackPlot"),
        #verbatimTextOutput("selecttext")
    )
)

## Create the Shiny Server layout
server <- function(input, output) {
    ## Create the plotly plot that compares price vs scoops
    output$TimePlot <- renderPlotly({
        timetemplate %>% 
            add_segments(data=D_event, name="Event", x =~TimeStamp, y=~labelpos-0.02, xend =~TimeStamp, yend =0, size=I(1), color=I("Gray")) %>%
            add_trace(data=D_event, name="Game Event Label", x =~TimeStamp, y =~labelpos, color = ~EventType, key=~rowID,
                      type='scattergl',mode='text', text=~Event, textfont = list(size = 8)) %>%
            event_register("plotly_selected")
    })
    
    ## Create the plotly plot of price vs rating based on selection
    output$WhackPlot <- renderPlotly({
        select.data <- event_data(event = "plotly_selected")
        if (!is.null(select.data)) {
            D_vis = D_event %>% filter(rowID %in% select.data$key)
        } else {
            D_vis = D_event
        }
        vistemplate %>%
            add_trace(data=Wall_moles,x=~x, y=~y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none') %>%
            add_trace(data=D_vis, x=~MoleIndexX-1, y=~MoleIndexY-1, type='scatter', mode='markers',marker=list(size=32)) %>%
        layout(
            xaxis=list(dtick = 1),
            yaxis=list(dtick = 1)
        )
    })

    #output$selecttext <- renderPrint({
    #    d <- event_data("plotly_selected")
    #    if (is.null(d)) "Click events appear here (double-click to clear)" else d
    #})
}

shinyApp(ui = ui, server = server)
