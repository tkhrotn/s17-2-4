library(shiny)
library(shinydashboard)
library(leaflet)
library(htmlwidgets)


# ---- 外部Shinyアプリケーションの起動 ----
library(callr)

bg1 <- r_bg(function (){
  library(shiny)
  runApp("gis/shinyapp", port = 10000)
})

while (TRUE) {
  if (sum(startsWith(bg1$read_error_lines(), "Listening")) > 0)
    break
}

bg2 <- r_bg(function (){
  library(shiny)
  runApp("air/shinyapp", port = 10002)
})

while (TRUE) {
  if (sum(startsWith(bg2$read_error_lines(), "Listening")) > 0)
    break
}

bg3 <- r_bg(function(){
  library(shiny)
  runApp("db/shinyapp", port = 10001)
})

while (TRUE) {
  if (sum(startsWith(bg3$read_error_lines(), "Listening")) > 0)
    break
}

bg4 <- r_bg(function(){
  library(shiny)
  #runApp("shinyapps/YDR_basin_2019_UN1118", port = 10003)
  runApp("YDR_basin_2023", port = 10003)
})

while (TRUE) {
  if (sum(startsWith(bg4$read_error_lines(), "Listening")) > 0)
    break
}


bg5 <- r_bg(function(){
  library(shiny)
  runApp("expo/shinyapp", port = 10004)
})

while (TRUE) {
  if (sum(startsWith(bg5$read_error_lines(), "Listening")) > 0)
    break
}

# ---- ShinyアプリケーションUI ----
ui <- dashboardPage(
  dashboardHeader(title = "統合プラットフォーム"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("ホーム", icon = icon("home"), tabName = "Home"),
      menuItem("基盤地図", icon = icon("map"), tabName = "ChemicalGIS"),
      menuItem("大気拡散マッピング", icon = icon("cloud"), tabName = "AirView"),
      menuItem("水域流出マッピング", icon = icon("signal"), tabName = "RiverView"),
      menuItem("健康影響情報", icon = icon("book"), tabName = "ChemicalHealthDatabase"),
      menuItem("曝露量の推計", icon = icon("male"), tabName = "Exposure")
      #menuItem("コホート情報", icon = icon("info"), tabName = "Disease")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              tags$div(width = "100%", style = "overflow-y:scroll",
                       box(title = "基盤地図",
                           p("化学物質に関連する施設等の地理情報視覚化ツール"),
                           img(src = "gis.png"),
                           shiny::actionButton(inputId='tab1', label="表示",
                                               icon = icon("play"))),
                       box(title = "大気拡散マッピング",
                           p("観測データから大気中の化学物質濃度を推計"),
                           img(src = "air.png"),
                           shiny::actionButton(inputId='tab2', label="表示",
                                               icon = icon("play"))),
                       box(title = "水域流出マッピング",
                           p("河川・地下水における化学物質流出マッピングツール"),
                           img(src = "basin.png"),
                           shiny::actionButton(inputId='tab3', label="表示",
                                               icon = icon("play"))),
                       box(title = "健康影響情報",
                           p("化学物質の健康影響情報"),
                           img(src = "db.png"),
                           shiny::actionButton(inputId='tab4', label="表示",
                                               icon = icon("play"))),
                       box(title = "曝露量推計",
                           p("化学物質濃度から人への曝露量を推計"),
                           img(src = "expo.png"),
                           shiny::actionButton(inputId='tab5', label="表示",
                                               icon = icon("play")))
                       )

              ),
      tabItem(tabName = "ChemicalGIS",
        tags$div(width = "100%", style = "margin:-14px;",
          tags$style(type = "text/css", "#ChemicalGIS_frame {height: calc(100vh - 58px) !important;}"),
          tags$iframe(id = "ChemicalGIS_frame", frameborder = 0, src = "http://127.0.0.1:10000", width = "100%", height = "100%")
      )),
      tabItem(tabName = "AirView",
        tags$div(width = "100%", style = "margin:-14px;",
          tags$style(type = "text/css", "#AirView_frame {height: calc(100vh - 58px) !important;}"),
          tags$iframe(id = "AirView_frame", frameborder = 0, src = "http://127.0.0.1:10002", width = "100%", height = "100%")
      )),
      tabItem(tabName = "RiverView",
        tags$div(width = "100%", style = "margin:-14px;",
          tags$style(type = "text/css", "#RiverView_frame {height: calc(100vh - 58px) !important;}"),
          tags$iframe(id = "RiverView_frame", frameborder = 0, src = "http://127.0.0.1:10003", width = "100%", height = "100%")
      )),
      tabItem(tabName = "ChemicalHealthDatabase",
        tags$div(width = "100%", style = "margin:-14px;",
          tags$style(type = "text/css", "#ChemicalHealthDatabase_frame {height: calc(100vh - 58px) !important;}"),
          tags$iframe(id = "ChemicalHealthDatabase_frame", frameborder = 0, src = "http://127.0.0.1:10001", width = "100%", height = "100%")
      )),
      tabItem(tabName = "Exposure",
              tags$div(width = "100%", style = "margin:-14px;",
                                  tags$style(type = "text/css", "#Exposure_frame {height: calc(100vh - 58px) !important;}"),
                                  tags$iframe(id = "Exposure_frame", frameborder = 0, src = "http://127.0.0.1:10004", width = "100%", height = "100%")
              ))
    )
  )
)

# Define server logic
server <- function(input, output, clientData, session) {
  observeEvent(input$tab1, {
    updateTabItems(session, "tabs","ChemicalGIS")
  })
  
  observeEvent(input$tab2, {
    updateTabItems(session, "tabs","AirView")
  })
  
  observeEvent(input$tab3, {
    updateTabItems(session, "tabs","RiverView")
  })
  
  observeEvent(input$tab4, {
    updateTabItems(session, "tabs","ChemicalHealthDatabase")
  })
  
  observeEvent(input$tab5, {
    updateTabItems(session, "tabs","Exposure")
  })
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}

shinyApp(ui, server)
