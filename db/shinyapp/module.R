#' UI of ChemicalHealthDatabase
ChemicalHealthDatabaseUI <- function(id) {
  require(DT)
  ns <- NS(id)
  
  js <- sprintf('
    function OnLinkClick(data) {
      Shiny.setInputValue("%s", data, {priority: "event"});
      return false;
    }
    ', ns("Link"))
  
  fluidRow(
    # tags$style("
    #           body {
    #           -moz-transform: scale(1.2, 1.2); /* Moz-browsers */
    #           zoom: 1.2; /* Other non-webkit browsers */
    #           zoom: 120%; /* Webkit browsers */
    #           }"),
    column(
      width = 12,
      shinydashboard::box(
        title = "化学物質名で検索",
        textInput(ns("Search"), label = NULL, width = "100%"),
        collapsible = TRUE,
        width = NULL
      ),
      shinydashboard::box(
        title = "症状による絞り込み（AND検索）",
        checkboxGroupInput(ns("Symptoms"),
                           label = NULL,
                           choiceNames = list(p(img(src = "icon_respiratory.png", width = 50, height = 50), "呼吸器症状"),
                                              p(img(src = "icon_eye.png", width = 50, height = 50), "眼症状"),
                                              p(img(src = "icon_skin.png", width = 50, height = 50), "皮膚症状"),
                                              p(img(src = "icon_digestive.png", width = 50, height = 50), "消化器症状"),
                                              p(img(src = "icon_cancer.png", width = 50, height = 50), "がん")),
                           choiceValues = c("Respiratory", "Eye", "Skin", "Digestive", "Cancer"),
                           inline = TRUE),
        collapsible = TRUE,
        collapsed = TRUE,
        width = NULL
      ),
      shinydashboard::box(
        tags$head(tags$script(HTML(js))),
        div(DT::dataTableOutput(ns("Table"), width = "100%"), style = "font-size: 120%"),
        status = "primary",
        width = NULL
      )
    )
  )
}

#' Server logic of ChemicalHealthDatabase
ChemicalHealthDatabaseServer <- function(id) {
  require(dplyr)
  require(stringr)
  moduleServer(
    id,
    function(input, output, session) {
      require(shiny)
      require(shinydashboard)
      require(DT)
      
      dataset <- readRDS("data/CH.rds")
      
      ch <- reactive({
        retval <- dataset
        
        if (input$Search != "") {
          retval <- retval %>% filter(str_detect(`化学物質`, input$Search))
        }
        
        if ("Respiratory" %in% input$Symptoms) {
          retval <- retval %>% filter(str_detect(`GHS分類結果`, "吸入"))
        }
        
        if ("Eye" %in% input$Symptoms) {
          retval <- retval %>% filter(str_detect(`GHS分類結果`, "眼"))
        }

        if ("Skin" %in% input$Symptoms) {
          retval <- retval %>% filter(str_detect(`GHS分類結果`, "皮膚"))
        }

        if ("Digestive" %in% input$Symptoms) {
          retval <- retval %>% filter(str_detect(`GHS分類結果`, "飲み込む"))
        }
        
        if ("Cancer" %in% input$Symptoms) {
          retval <- retval %>% filter(str_detect(`GHS分類結果`, "発がん"))
        }
        
        return(retval)
      })
      
      output$Table = DT::renderDataTable(
        datatable(
          ch(),
          escape = FALSE,
          #filter = "top",
          selection = "none",rownames = FALSE,style = "bootstrap",
          
          options = list(dom = "ltipr",
                         lengthMenu = list(c(10, 25, 50, -1),
                                           c('10', '25', '50', 'All')),
                         pageLength=25,
                         scrollY = "calc(100vh - 500px)",
                         autoWidth = TRUE,
                         columnDefs = list(list(width = '200px', targets = 0),
                                           list(width = '300px', targets = 2),
                                           list(width = '70px', targets = 3:4)))), server = T)
      
      observeEvent(input$Link, {
        args <- strsplit(input$Link, "&")[[1]]
        showModal(modalDialog(
          title = paste(args[1], "の健康影響", sep = ""),
          size = "l",
          footer = modalButton("閉じる"),
          tags$iframe(src=args[2], height=600, width="100%")
        ))
      })
      
      # HTTP GETクエリの処理
      observe({
        query <- getQueryString()
        
        if (!is.null(query$chem)) {
          updateTextInput(session, inputId = "Search", value = query$chem)
        }
      })
    }
  )
}

