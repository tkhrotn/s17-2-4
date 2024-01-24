library(shiny)
library(shinydashboard)
library(shinyjs)
library(htmlwidgets)

source("module.R", encoding = "UTF8")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "健康影響情報"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(ChemicalHealthDatabaseUI("ch")),
  skin = "black"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ChemicalHealthDatabaseServer("ch")
}

# Run the application 
shinyApp(ui = ui, server = server)
