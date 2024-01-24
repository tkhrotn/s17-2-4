install.packages("shiny")
install.packages("leaflet") 
install.packages("devtools")
devtools::install_github("rstudio/leaflet")
# install.packages("later")

# Deploy libraries
library(shiny)
library(leaflet)
library(devtools)
library(later)

# Run the application 
shinyApp(ui = ui, server = server)
