AirViewUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(
      tabPanel("地図",
               leafletOutput(ns("Map"),width = "100%", height = 580),
               textOutput(ns("SelectedLocation")),
               plotOutput(ns("Plot"), width = "100%", height = 200),
      ),
      tabPanel("数値データ",
               dataTableOutput(ns("DataTable"))
      ),
      width = 9
    ),
    tabBox(
      tabPanel("ファイルの読み込み",
               fileInput(ns("FileInput"), "ファイルの選択", accept = c(".xlsx, .xls, .csv"), buttonLabel = "開く...", placeholder = "ファイルが選択されていません。"),
               textOutput(ns("SelectedTime")),
               uiOutput(ns("TimeRangeInput"))
               ),
      tabPanel("点の追加",
               textInput(ns("LocationNameInput"), label = "地点名"),
               textInput(ns("UnitInput"), label = "単位"),
               numericInput(ns("ValueInput"), label = "値", value = NULL),
               actionButton(ns("RegisterButton"), label = "登録", icon = icon("edit"))
               ),
      width = 3
    )
  )
}


AirViewServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      require(ggplot2)
      require(DT)
      
      source("modules/AirView/read-file.R", local = TRUE)
      source("modules/AirView/gis.R", local = TRUE)
      
      rval <- reactiveValues(dataset = NULL, count = 1)
      
      output$Map <- renderLeaflet({
        leaflet() %>% addTiles("http://cyberjapandata.gsi.go.jp/xyz/blank/{z}/{x}/{y}.png", group = "白地図", layerId = "blank", attribution = "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>国土地理院</a>",  options = tileOptions(zIndex = 0)) %>% 
          addTiles("https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png", group = "標準地図", layerId = "std", attribution = "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>国土地理院</a>", options = tileOptions(zIndex = 0)) %>% 
          setView(140, 37, zoom = 7) %>% 
          addLayersControl(baseGroups = c("白地図", "標準地図"), overlayGroups = c("最近傍法による推計", "逆距離加重法による推計", "通常クリギング法による推計"), options=layersControlOptions(collapsed = FALSE, autoZIndex = FALSE))
      })
      
      output$Plot <- renderPlot({
        click <- input$Map_marker_click
        if (is.null(click))
          return(ggplot() + geom_point())
        
        selected.data <- rval$dataset[rval$dataset$location_name == unlist(strsplit(click$id, ","))[2],]
        if (!is.null(rval$dataset)) {
          ggplot(selected.data, aes(time_code, value)) + geom_vline(xintercept=input$TimeRange,colour="#FF9999") + scale_x_continuous(breaks=selected.data$time_code,labels=selected.data$time_name)+ geom_point(color="#6666FF") + geom_line(color="#6666FF") + labs(x="日時", y="空間線量率 (micro Sv/h)")
          #ggplot(selected.data, aes(time_code, value)) + geom_vline(xintercept=input$TimeRange,colour="#FF9999") + scale_x_continuous(breaks=selected.data$time_code,labels=selected.data$time_name)+ geom_point(color="#6666FF") + geom_line(color="#6666FF") + labs(x="日時", y="Cs137土壌沈着量 (Bq/m^2)")
        } else {
          ggplot() + geom_point()
        }
      })
      
      output$TimeRangeInput <- renderUI({
        if (!is.null(rval$dataset)) {
          t.min <- min(rval$dataset$time_code)
          t.max <- max(rval$dataset$time_code)
          sliderInput(session$ns("TimeRange"), NULL, max = t.max, min = t.min, value = t.min, step = 1, animate = TRUE, ticks = FALSE)
        } else {
          sliderInput(session$ns("TimeRange"), NULL, max = 0, min = 0, value = 0, step = 1, ticks = FALSE)
        }
      })
      
      output$DataTable <- renderDataTable({
        datatable(rval$dataset, editable = TRUE, filter = 'top')
      })
      
      output$SelectedTime <- renderText({
        data <- rval$dataset
        selected.data <- data[data$time_code == input$TimeRange,]
        as.character(selected.data$time_name[1])
      })
      
      output$SelectedLocation <- renderText({
        click <- input$Map_marker_click
        if (is.null(click)) {
          return("グラフ")
        } else {
          return(unlist(strsplit(click$id, ","))[2])
        }
      })
      
      
      observeEvent(input$FileInput,{
        map <- leafletProxy("Map")
        map <- map %>% clearMarkers() %>% clearMarkerClusters() %>% clearImages() %>% clearControls()
        
        rval$dataset <- readFile(input$FileInput$datapath)
        
        data <- rval$dataset
        times <- unique(sort(data$time_code))
        
        pal <- colorBin(circle.palette, c(-1, 10000000), bins = c(-1, data.bins, 10000000), na.color = "transparent", alpha = TRUE)
        
        for (i in 1:length(times)) {
          selected.data <- data[data$time_code == times[i],]
          selected.data <- selected.data[!is.na(selected.data$value),]
          map <- map %>% addCircleMarkers(lng = selected.data$longitude,
                                          lat = selected.data$latitude,
                                          label = selected.data$location_name,
                                          color = rgb(0,0,0), opacity = 1, weight = 1, radius = 4,
                                          fillColor = pal(selected.data$value),
                                          fillOpacity = 1,
                                          layerId = sprintf("Marker,%s,%d", selected.data$location_name, times[i]),
                                          group = as.character(selected.data$time_code))  %>% hideGroup(as.character(selected.data$time_code))
        }
        
        map %>% leaflet::addLegend(position = "bottomright", opacity = 1.0, colors = circle.palette, labels = data.labels) %>% showGroup(as.character(times[1]))
      })
      
      
      observeEvent(input$Map_click, {
        click <- input$Map_click
        leafletProxy("Map") %>% addMarkers(lng = click$lng, lat = click$lat, layerId = "ClickedMarker")
      })
      
      observeEvent(input$Map_marker_click, {
        click <- input$Map_marker_click
        if (is.null(click))
          return()
        
        if (!is.null(rval$dataset$id)) {
          name <- rval$dataset[rval$dataset$id == click$id,"location_name"]
          updateTextInput(session, "LocationNameInput", value = name)
        }
      })
      
      ras_nn <- reactiveVal()
      
      observe({
        d <- input$TimeRange
        selected.data <- rval$dataset[rval$dataset$time_code == d,]
        future({
          racdaNearestNeighbour(as.data.frame(selected.data), 50, arrow.log = TRUE)
        }) %...>%
          ras_nn()
      })
      
      runInterpolate <- function() {
        d <- input$TimeRange
        if (is.null(d) | is.null(rval$dataset))
          return()
        
        selected.data <- rval$dataset[rval$dataset$time_code == d,]
        
        map <- leafletProxy("Map")
        map <- map %>% clearImages()
        
        ras <- racdaNearestNeighbour(as.data.frame(selected.data), 200, arrow.log = TRUE)
        if (!is.null(ras)) {
          pal <- colorBin(pred.palette, raster::values(ras), bins = c(-1, data.bins, 10000000), na.color = "transparent", alpha = TRUE)
          if (!is.null(ras)) {
            map <- map %>% addRasterImage(ras, group = "最近傍法による推計", colors=pal, opacity = 1, project = FALSE)
          }
        }
        
        ras <- racdaIDW(as.data.frame(selected.data), 200, arrow.log = TRUE)
        if (!is.null(ras)) {
          pal <- colorBin(pred.palette, raster::values(ras), bins = c(-1, data.bins, 10000000), na.color = "transparent", alpha = TRUE)
          if (!is.null(ras)) {
            map <- map %>% addRasterImage(ras, group = "逆距離加重法による推計", colors=pal, opacity = 1, project = FALSE)
          }
        }
        
        ras <- racdaOrdinaryKriging(as.data.frame(selected.data), 100, arrow.log = TRUE)
        if (!is.null(ras)) {
          pal <- colorBin(pred.palette, raster::values(ras), bins = c(-1, data.bins, 10000000), na.color = "transparent", alpha = TRUE)
          if (!is.null(ras)) {
            map <- map %>% addRasterImage(ras, group = "通常クリギング法による推計", colors=pal, opacity = 1, project = FALSE)
          }
        }
        
        # ras <- racdaSimpleKriging(as.data.frame(selected.data), 100, arrow.log = TRUE)
        # if (!is.null(ras)) {
        #   pal <- colorBin(pred.palette, values(ras), bins = c(-1, data.bins, 10000000), na.color = "transparent", alpha = TRUE)
        #   if (!is.null(ras)) {
        #     map <- map %>% addRasterImage(ras, group = "単純クリギング法による推計", colors=pal, opacity = 1, project = FALSE)
        #   }
        # }
      }
      
      observe({
        map <- leafletProxy("Map")
        map <- map %>% clearImages()
        ras <- ras_nn()
        if (!is.null(ras)) {
          pal <- colorBin(pred.palette, raster::values(ras), bins = c(-1, data.bins, 10000000), na.color = "transparent", alpha = TRUE)
          map <- map %>% addRasterImage(ras, group = "通常クリギング法による推計", colors=pal, opacity = 1, project = FALSE)
        }
      })
      
      # observeEvent(input$RegisterButton, {
      #   click <- input$Map_click
      #   t <- input$TimeRange
      #   
      #   if (is.null(click) | is.null(t))
      #     return()
      #   
      #   t_name <- rval$dataset[rval$dataset$time_code == t,]$time_name[1]
      #   
      #   rval$dataset <- rbind(rval$dataset, data.frame(time_code = t, time_name = t_name, location_name = input$LocationNameInput, latitude = click$lat, longitude = click$lng, unit = input$UnitInput, value = input$ValueInput))
      #   
      #   pal <- colorBin(circle.palette, c(-1, 10000000), bins = c(-1, data.bins, 10000000), na.color = "transparent", alpha = TRUE)
      #   leafletProxy("Map") %>% addCircleMarkers(lng = click$lng,
      #                                            lat = click$lat,
      #                                            label = input$LocationNameInput,
      #                                            color = rgb(0,0,0), opacity = 1, weight = 1, radius = 4,
      #                                            fillColor = pal(input$ValueInput),
      #                                            fillOpacity = 1,
      #                                            layerId = sprintf("Marker,%s,%d", input$LocationNameInput, t),
      #                                            group = as.character(t))
      #   
      #   runInterpolate()
      # })
      # 
      # observeEvent(input$TimeRange, {
      #   d <- input$TimeRange
      #   if (is.null(d) | is.null(rval$dataset))
      #     return()
      #   
      #   map <- leafletProxy("Map")
      #   times <- unique(rval$dataset$time_code)
      #   for (i in 1:length(times)) {
      #     if (times[i] == d) {
      #       map <- map %>% showGroup(as.character(times[i]))
      #     } else {
      #       map <- map %>% hideGroup(as.character(times[i]))
      #     }
      #   }
      #   
      #   runInterpolate()
      # })
    }
  )
}
