library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(htmlwidgets)

source("module/module.R", encoding = "UTF8")

# Define UI for application that draws a histogram
ui <- AirViewUI("airview")

# Define server logic required to draw a histogram
server <- function(input, output) {
  AirViewServer("airview")
}

# Run the application 
shinyApp(ui = ui, server = server)
