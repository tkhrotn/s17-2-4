AirViewUI <- function(id) {
  require(DT)
  require(dipsaus)
  require(shinyBS)
  require(esquisse)
  require(scales)
  require(sortable)
  require(shinyWidgets)
  require(RColorBrewer)
  require(plotly)
  
  ns <- NS(id)
  
  navbarPage(
    "大気拡散マッピング",
    fluidRow(
      column(
        width = 9,
        tabsetPanel(type = "tabs",
                    tabPanel("マップ", icon = icon("map"),
                             tags$head(includeCSS("styles.css")),
                             div(
                               leafletOutput(ns("Map"), height = "100%"),
                               style = "height: calc(100vh - 120px)"
                             )
                    ),
                    tabPanel("地点データ", icon = icon("map-marker"),
                             div(
                               DT::dataTableOutput(ns("DataTable"), height = "100%"),
                               style = "height: calc(100vh - 165px)"
                             )
                             
                    ),
                    tabPanel(
                      "補間データ", icon = icon("area-chart"),
                      div(
                        DT::dataTableOutput(ns("InterpolateTable"), height = "100%"),
                        style = "height: calc(100vh - 165px)"
                      )
                    ),
                    tabPanel(
                      "グループ", icon = icon("object-group"),
                      div(
                        DT::dataTableOutput(ns("GroupTable"), height = "100%"),
                        style = "height: calc(100vh - 165px)"
                      )
                    )
        )
      ),
      column(
        width = 3,
        bsCollapse(
          id = ns("Collapse"),
          bsCollapsePanel(
            title = "ファイルから読み込む", 
            style = "primary",
            fileInput(ns("FileInput"), label = NULL, accept = c(".xlsx, .xls, .csv"), buttonLabel = "開く...", placeholder = "ファイルが選択されていません。", width = "100%")
          ),
          bsCollapsePanel(
            title = "新しい地点データの登録",
            style = "primary",
            p("緯度・経度は地図をクリックすると自動的に入力されます。"),
            fluidRow(
              column(6, numericInput(ns("Latitude"), value = NULL, label = "緯度")),
              column(6, numericInput(ns("Longitude"), value = NULL, label = "経度"))
            ),
            textInput(ns("LocationNameInput"), label = "地点名", width = "100%"),
            textInput(ns("GroupNameInput"), label = "グループ名", width = "100%"),
            numericInput(ns("ValueInput"), label = "データ", value = NULL, width = "100%"),
            textInput(ns("UnitInput"), label = "備考", width = "100%"),
            actionButtonStyled(ns("RegisterButton"), label = "登録", icon = icon("edit"), width = "100%")
          ),
          bsCollapsePanel(
            title = "空間補間データの作成",
            style = "primary",
            p("空間補間データは同一グループ内の地点データを集約して作成されます。グループの設定を変更する場合は「地点データ」を開き、該当する地点データを編集してください。"),
            pickerInput(
              ns("InterpolationSelect"),
              label = "表示するグループ",
              choices = NULL,
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      noneSelectedText = "グループを選択",
                                      selectAllText = "すべて選択",
                                      deselectAllText = "選択解除"),
              width = "100%"
            ),
            selectInput(
              ns("InterpolationMethod"),
              label = "補間手法を選択",
              choices = c("通常クリギング法" = "ok",
                          "逆距離荷重法" = "idw",
                          "最近傍法" = "nn"),
              multiple = FALSE,
              width = "100%"
            ),
            actionButtonStyled(
              ns("InterpolationButton"),
              label = "空間補間を実行",
              icon = icon("play"),
              width = "100%"
            )
          ),
          bsCollapsePanel(
            title = "スタイルの設定",
            style = "primary",
            palettePicker(
              ns("Palette"),
              label = "パレット",
              choices = list(
                "Reds" = brewer.pal(8, "Reds"),
                "Blues" = brewer.pal(8, "Blues"),
                "Greens" = brewer.pal(8, "Greens"),
                "Greys" = brewer.pal(8, "Greys"),
                "Spectral" = brewer.pal(8, "Spectral")
              )
            ),
            fluidRow(
              column(6, numericInput(ns("StyleMin"), label = "最小値", value = NA)),
              column(6, numericInput(ns("StyleMax"), label = "最大値", value = NA))
            ),
            p("最小値・最大値を超えるデータは透明色で表示されます。"),
            numericInput(
              ns("Breaks"),
              label = "階級の数",
              value = 5,
              min = 1,
              width = "100%"
            ),
            selectInput(
              ns("Classifier"),
              label = "分類方法",
              choices = c("等量（Quantile）" = "quantile",
                          "等間隔" = "equidistant"),
              width = "100%"
            ),
            numericInput(
              ns("CircleSize"),
              label = "マーカーの大きさ（px）",
              value = 5,
              min = 1,
              width = "100%"
            )
          ),
          bsCollapsePanel(
            title = "アニメーション表示",
            style = "primary",
            pickerInput(
              ns("AnimationSelect"),
              label = "表示するグループ",
              choices = NULL,
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      noneSelectedText = "グループを選択",
                                      selectAllText = "すべて選択",
                                      deselectAllText = "選択解除"),
              width = "100%"
            ),
            h5("表示する順番（上から順に表示）"),
            uiOutput(ns("AnimationOrder")),
            numericInput(
              ns("AnimationInterval"),
              label = "アニメーション間隔（ミリ秒）",
              value = 1000,
              min = 1,
              width = "100%"
            ),
            sliderInput(
              ns("Animation"),
              label = "再生",
              min = 1,
              max = 1,
              value = 1,
              width = "100%",
              animate = animationOptions()
            )
          )
        )
      )
    )
    
  )
}


AirViewServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      require(ggplot2)
      require(DT)
      require(sortable)
      require(leafpop)
      require(tibble)
      require(stringr)
      
      source("module/read-file.R", local = TRUE, encoding = "UTF8")
      source("module/gis.R", local = TRUE, encoding = "UTF8")
      
      base_groups <- c("OpenStreetMap", "淡色地図", "航空写真", "白地図")
      
      # reactive変数 -----------------------------------------------------------
      rval <- reactiveValues(
        dataset = NULL,
        group = NULL,
        interpolation = NULL)
      
      output$Map <- renderLeaflet({
        leaflet() %>%
          addMapPane("baseMapPane", 0) %>%
          addTiles(group = base_groups[1]) %>%
          addTiles(
            "https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png",
            group = base_groups[2],
            attribution = "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>国土地理院</a>",
            options = tileOptions(pane = "baseMapPane", fadeAnimation = FALSE)) %>%
          addTiles(
            "https://cyberjapandata.gsi.go.jp/xyz/seamlessphoto/{z}/{x}/{y}.jpg",
            group = base_groups[3],
            attribution = "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>国土地理院</a>",
            options = tileOptions(pane = "baseMapPane", fadeAnimation = FALSE)) %>%
          addTiles(
            "https://cyberjapandata.gsi.go.jp/xyz/blank/{z}/{x}/{y}.png",
            group = base_groups[4],
            attribution = "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>国土地理院</a>",
            options = tileOptions(pane = "baseMapPane", fadeAnimation = FALSE)) %>% 
          setView(140, 37, zoom = 7) %>% 
          addLayersControl(
            baseGroups = base_groups,
            position = "topright",
            options=layersControlOptions(autoZIndex = FALSE, collapsed = FALSE)) %>%
          addMiniMap(position = "bottomleft")
      })
      
      output$DataTable <- renderDT(
        isolate(rval$dataset),
        editable = TRUE,
        filter = 'top',
        option = list(dom = "ltipr",
                      lengthMenu = list(c(10, 25, 50, -1),
                                        c('10', '25', '50', 'All')),
                      pageLength=25,
                      scrollY = "calc(100vh - 350px)",
                      autoWidth = TRUE)
      )
      
      observeEvent(input$DataTable_cell_edit, {
        row <- input$DataTable_cell_edit$row
        col <- input$DataTable_cell_edit$col
        rval$dataset[row, col] <- input$DataTable_cell_edit$value
        pxy <- dataTableProxy("DataTable")
        replaceData(pxy, rval$dataset, resetPaging = FALSE)
        
        group <- unique(rval$dataset$group_name)
        print(group)
        rval$group <- tibble(group = group)
      })
      
      output$GroupTable <- renderDT({
        datatable(
          rval$group,
          editable = FALSE,
          filter = 'top',
          option = list(dom = "ltipr",
                        lengthMenu = list(c(10, 25, 50, -1),
                                          c('10', '25', '50', 'All')),
                        pageLength=25,
                        scrollY = "calc(100vh - 350px)",
                        autoWidth = TRUE))
      })
      
      # ファイルの読み込み -----------------------------------------------------
      observeEvent(input$FileInput,{
        map <- leafletProxy("Map")
        map <- map %>% clearMarkers() %>% clearMarkerClusters() %>% clearImages() %>% clearControls()
        
        rval$dataset <- readFile(input$FileInput$datapath)
        group <- unique(rval$dataset$group_name)
        rval$group <- tibble(group = group)
        
        map %>% hideGroup(group[-1]) -> map
      })
      
      updateMapObjects <- function() {
        map <- leafletProxy("Map")
        map %>%
          clearMarkers() %>%
          clearControls() -> map
        
        trimed <- rval$dataset
        
        if (!is.na(input$StyleMin)) {
          trimed %>% filter(value > input$StyleMin) -> trimed
        }
        
        if (!is.na(input$StyleMax)) {
          trimed %>% filter(value < input$StyleMax) -> trimed
        }
        
        switch(input$Classifier,
          "quantile" = {
            breaks <- quantile(trimed$value, seq(0, 1, 1 / input$Breaks), na.rm = TRUE)
          },
          "equidistant" = {
            mi <- min(trimed$value, na.rm = TRUE)
            ma <- max(trimed$value, na.rm = TRUE)
            breaks <- seq(mi, ma, (ma - mi) / input$Breaks)
          }
        )
        
        pal <- brewer.pal(input$Breaks, input$Palette)
        
        if (!is.na(input$StyleMin) & input$StyleMin > min(rval$dataset$value, na.rm = TRUE)) {
          breaks <- c(input$StyleMin, breaks)
          pal <- c("#FFFFFFFF", pal)
        }

        if (!is.na(input$StyleMax) & input$StyleMax < max(rval$dataset$value, na.rm = TRUE)) {
          breaks <- c(breaks, input$StyleMax)
          pal <- c(pal, "#FFFFFFFF")
        }
        
        print(breaks)
        print(pal)

        palette <- colorBin(pal, rval$dataset$value, bins = breaks, na.color = "transparent", alpha = TRUE)
        
        
        for (i in rval$group$group) {
          rval$dataset %>% 
            filter(group_name == i & !is.na(value)) -> data

          if (nrow(data) > 0) {
            map %>%
              addCircleMarkers(
                data = data,
                lng = ~longitude,
                lat = ~latitude,
                radius = input$CircleSize,
                fillColor = ~palette(value),
                fillOpacity = 1,
                weight = 1,
                color = "black",
                opacity = 1,
                group = i,
                label = ~location_name,
                layerId = paste0(data$group_name, ":", data$location_name)) -> map
          }
        }
        
        lapply(rval$interpolation, function(x) {
          cbin <- colorBin(pal, raster::values(x$ras), bins = breaks, na.color = "transparent", alpha = TRUE)
          map %>% addRasterImage(x$ras, group = x$group, colors=cbin, opacity = 0.8, project = FALSE, layerId = paste0(x$group, ":", x$method)) -> map
        })
        
        map %>%
          addLayersControl(
            baseGroups = base_groups,
            overlayGroups = rval$group$group,
            position = "topright",
            options=layersControlOptions(autoZIndex = FALSE, collapsed = FALSE)) %>%
          addMiniMap(position = "bottomleft") -> map
        
        if (!is.null(rval$group)) {
          map %>% 
            leaflet::addLegend(
              position = "bottomleft",
              colors = pal,
              labels = paste(breaks[-length(breaks)], "-")
            ) -> map
        }
      }
      
      observe({
        updateMapObjects()
      })
      
      observe({
        updatePickerInput(session = session, inputId = "InterpolationSelect", choices = rval$group$group)
        updatePickerInput(session = session, inputId = "AnimationSelect", choices = rval$group$group)
        updateNumericInput(
          session = session,
          inputId = "StyleMin",
          min = min(rval$dataset$value, na.rm = TRUE),
          max = max(rval$dataset$value, na.rm = TRUE),
          value = min(rval$dataset$value, na.rm = TRUE)
        )
        updateNumericInput(
          session = session,
          inputId = "StyleMax",
          min = min(rval$dataset$value, na.rm = TRUE),
          max = max(rval$dataset$value, na.rm = TRUE),
          value = max(rval$dataset$value, na.rm = TRUE)
        )
      })
      
      # 空間補間 ---------------------------------------------------------------
      observeEvent(input$InterpolationButton, {
        map <- leafletProxy("Map")
        
        withProgress(message = "補間データ作成中", value = 0, {
          for (i in input$InterpolationSelect) {
            selected.data <- rval$dataset %>% filter(group_name == i)
            
            ras <- switch (input$InterpolationMethod,
                           "idw" = {
                             racdaIDW(as.data.frame(selected.data), 200, arrow.log = TRUE)
                           },
                           "nn" = {
                             racdaNearestNeighbour(as.data.frame(selected.data), 200, arrow.log = TRUE)
                           },
                           "ok" = {
                             racdaOrdinaryKriging(as.data.frame(selected.data), 100, arrow.log = TRUE)
                           }
            )
            
            if (is.null(rval$interpolation)) {
              rval$interpolation <- list(list(group_name = i, method = input$interpolationMethod, ras = ras))
            } else {
              rval$interpolation <- c(rval$interpolation, list(list(group_name = i, method = input$interpolationMethod, ras = ras)))
            }
            
            incProgress(1/length(input$InterpolationSelect), detail = i)
          }
        })
      })
      
      # アニメーション表示 -----------------------------------------------------
      output$AnimationOrder <- renderUI({
        wellPanel(
          style = "overflow-y:scroll; max-height: 250px; padding: 0;",
          rank_list(
            text = NULL,
            labels = input$AnimationSelect,
            input_id = session$ns("AnimationOrder")
          )
        )
      })
      
      observeEvent(input$AnimationSelect, {
        updateSliderInput(
          session = session,
          inputId = "Animation",
          value = 1,
          min = 1,
          max = length(input$AnimationSelect),
          step = 1)
        
        map <- leafletProxy("Map")
        for (i in rval$group$group) {
          map %>% hideGroup(i) -> map
        }
        
        map %>% showGroup(input$AnimationSelect[1]) -> map
      })
      
      observeEvent(input$Animation, {
        map <- leafletProxy("Map")
        for (i in rval$group$group) {
          map %>% hideGroup(i) -> map
        }
        
        map %>% showGroup(input$AnimationOrder[input$Animation]) -> map
      })

      observeEvent(input$Map_click, {
        click <- input$Map_click
        leafletProxy("Map") %>% addMarkers(lng = click$lng, lat = click$lat, layerId = "ClickedMarker")
        
        updateNumericInput(session = session, inputId = "Latitude", value = click$lat)
        updateNumericInput(session = session, inputId = "Longitude", value = click$lng)
      })
      
      # グラフ表示 -------------------------------------------------------------
      observeEvent(input$Map_marker_click, {
        click <- input$Map_marker_click
        if (is.null(click))
          return()
        
        loc <- str_replace(click$id, paste0(click$group, ":"), "")
        data <- rval$dataset %>% filter(location_name == loc)
        
        map <- leafletProxy("Map")
        map %>% addPopups(
          lng = click$lng,
          lat = click$lat,
          layerId = "PopupGraph",
          popup = popupGraph(
            {
              ggplot(data = data, aes(x = group_name, y = value)) +
                geom_bar(stat = "identity", fill = "#2792c3") + 
                labs(title = loc, x = "", y = "") -> p
              
              if(Sys.info()["sysname"] == "Darwin") {
                p + theme_grey(base_family = "HiraKakuProN-W3") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
              } else {
                p + theme_grey(base_family = "Noto Sans") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
              }
            },
            width = 500,
            height = 400, type = "png")
        ) -> map
      })
    }
  )
}
