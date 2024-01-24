library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(htmlwidgets)

source("module/server.R", encoding = "UTF8")
source("module/ui.R", encoding = "UTF8")

# Define UI for application that draws a histogram
ui <- ChemicalGISUI("chemicalgis")

# Define server logic required to draw a histogram
server <- function(input, output) {
  ChemicalGISServer("chemicalgis")
}

# Run the application 
shinyApp(ui = ui, server = server)
