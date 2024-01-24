library(shiny)
library(shinydashboard)
library(shinyjs)
library(htmlwidgets)

source("module.R", encoding = "UTF8")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "曝露量推計ツール"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(ExposureAssessmentUI("expo")),
  skin = "black"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ExposureAssessmentServer("expo")
}

# Run the application 
shinyApp(ui = ui, server = server)
