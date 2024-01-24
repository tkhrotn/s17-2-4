server <- function(input, output, clientData, session){

  source("mkMaps.R")

  geoJSON <- list()

  # TAB 1 ##################################################
  # TAB 1 REACTIVE ---------------------
  data <- reactive({
    req(input$csvfile)
    #data <- fread(file="data/YDR_basin2019.csv", encoding = 'UTF-8', dec=",")
    
    if (!grepl('.csv$', input$csvfile$datapath)) {
      reset('csvfile')
      return(NULL)
    } else {
      data <- fread(input$csvfile$datapath, encoding = 'UTF-8', dec = ",")
      data$VALUE <- as.numeric(data$VALUE)
      data$YMD <- as.Date(data$YMD)
      return(data)
    }
  })

  rainbowPalletSelected <- reactive({
    rainbowPalletSelected <- input$rainbowPalletSelected
    rainbowPalletSelected <- as.vector(gsub(".+\\s+", "", rainbowPalletSelected))
    return(rainbowPalletSelected)
  })

  map <- reactive({
    req(input$shpfile)
    shpdf <- input$shpfile
    geoJSON <<- list()

    tmpdirname <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      isJSON <- grepl("\\.json$", shpdf$name[i])
      isGeoJSON <- grepl('\\.geojson$', shpdf$name[i])
      isZIP <- grepl('.zip$', shpdf$name[i])
      if (isJSON) {
        tryCatch({
          topoJsonData <- geojsonio::geojson_read(shpdf$datapath[i])
          session$sendCustomMessage('onTopoJSON', topoJsonData)
        }, error = function (err) {
          print(paste('Error read json file: ', err))
        })
      } else if (isGeoJSON) {
        tryCatch({
          geoJsonData <- readOGR(shpdf$datapath[i])
          geoJSON[[length(geoJSON) + 1]] <<- geoJsonData
        }, error = function (err) {
          print(paste('Error read json file: ', err))
        })
      } else if (isZIP) {
        next
      } else {
        file.rename(shpdf$datapath[i], paste0(tmpdirname, "/", shpdf$name[i]))
      }
    }
    shpdfName <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
    if (identical(shpdfName, character(0))) {
      return(NULL)
    } else {
      #tmpdirname ="D:\\tmp\\YDR_basin_2019_UN1110\\data"
      #map <- readOGR(paste(tmpdirname, "YodogawaStations4a.shp",sep="/"))
      map <- readOGR(paste(tmpdirname, shpdfName, sep = "/"))
    }

  })

  # Selectable maps
  mapNo <- reactive({mapNo <- input$radio; return(mapNo) })
  srclat <- reactive({srclat <- input$map_click$lat; return(srclat); })
  srclng <- reactive({srclng <- input$map_click$lng; return(srclng); })
  srcROI <- reactive({srcROI <- input$srcROI; return(srcROI); })

  # TAB 1 FUNCTION ---------------------

  observe({
    data <- data()
    dates <- as.Date(data$YMD)
    min <- min(dates)
    max <- max(dates)
    updateDateInput(
      session = session,
      inputId = 'currentDate',
      value = min,
      min = min,
      max = max
    )
  })

  summaryByItem <- function(item) {
    data <- data()
    filter <- data[data$ITEM == item, 5]
    filter <- data.matrix(filter) # Convert character to numberic with data.matrix
    if (length(filter) == 0)
      summaryResult <- summary(0)
    else
      summaryResult <- summary(as.vector(filter)) # Convert matrix to vector
  }

  # TAB 1 OUTPUT ---------------------
  #(not used)
  output$latlngOutput = renderText({
    srclat <- srclat()
    srclng <- srclng()
    paste0(srclat, "     ", srclng)
  })
  
  output$latbox <- renderInfoBox({
    srclat <- srclat()
    infoBox(
      "緯度", srclat, icon = icon("map", lib = "font-awesome"),
      color = "light-blue", fill =TRUE, width = 3)
  }) 
  
  output$lngbox <- renderInfoBox({
    srclng <- srclng()
    infoBox(
      "緯度", srclng, icon = icon("map", lib = "font-awesome"),
      color = "light-blue", fill = TRUE, width = 3)
  })

  output$histRiver <- renderPlot({
    data <- data()
    if (is.null(data)) {
      return(NULL)
    }
    targetDate <- input$currentDate
    targetItem <- input$variableselected
    onDate <- as.Date(data$YMD)
    dataFiltered <- data[which(onDate == targetDate), ]
    item <- dataFiltered$ITEM
    dataFiltered <- data[which(item == targetItem), ]
    hist(
      as.numeric(dataFiltered$VALUE),
      main = paste0("Date: ", targetDate),
      xlab = targetItem,
      col = '#00DD00',
      cex.main = 1.2,
      cex.axis = 1.2,
      cex.lab = 1.2,
      border = 'white'
    )
  })

  #Not used now
  output$histSupply <- renderPlot({
    water_supply0 <- read.csv("data/water_supplu_H24(H22).csv", fileEncoding = "SJIS")
    hist(
      as.numeric(water_supply0$'日最大給水量'/1000),
      main = "Max. water supply",
      xlab = "日最大給水量 (X 1000 m3)",
      breaks = seq(0,2000,250),
      col = '#00DD00',
      cex.main = 1.2,
      cex.axis = 1.2,
      cex.lab = 1.2,
      border = 'white'
    )
  })

  output$map <- renderLeaflet({
    # Primary display multiple map
    if (is.null(data()) | is.null(map())) {
      return(NULL)
    }
    map <- map()
    data <- data()


    lavel_temp <- list(
     "color" = "yellow",
     #"font-family" = "arial",
     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
     "font-size" = "20px",
     "border-color" = "rgba(0,0,0,0.5)",
     "background-color" = "black",
     "opacity" = 0.65
    )

    m <- leaflet(map) %>%
      addTiles(basemap, attribution = til) %>%  
      addMiniMap(toggleDisplay = TRUE, position = "bottomleft") %>%
      leaflet::addScaleBar(position = "bottomright")
    
      for (index in seq_along(geoJSON)) {
        m <- addPolygons(
          m,
          data = geoJSON[[index]]
        )
      }
      m  %>%
      addPolygons(
        data = ws_population,
        fillColor = ~pal_ws_population(ws_population$P21A_004r), #col = 'dodgerblue',
        stroke = FALSE,
        fillOpacity = 0.3,
        smoothFactor = 0.5,
        group="給水人口") %>%
      
        addCircleMarkers(
          lng = ~wells$X,
          lat = ~wells$Y,
          #fillColor = "blue",
          radius = 5.,
          color = "cyan",
          dashArray = "2",
          label = labels_wells,
          group = "水道用井戸",
          labelOptions = labelOptions(
            noHide = F,
            direction = "bottom",
            style = lavel_temp,
            layerId = "水道用井戸"
          )
        ) %>%

        addCircleMarkers(
          lng = ~sewage_facility$X,
          lat = ~sewage_facility$Y,
          #fillColor = "blue",
          radius = 5.,
          color = "green",
          dashArray = "2",
          label = labels_sewage_facility,
          group = "下水施設",
          labelOptions = labelOptions(
            noHide = F,
            direction = "bottom",
            style = lavel_temp,
            layerId = "下水施設"
          )
        ) %>%

        addCircleMarkers(
          lng = water_supply$X,
          lat = water_supply$Y,
          #fillColor = "blue",
          radius = 5.,
          color = "#ffa500",
          dashArray = "2",
          label = labels_watersupply,
          group = "上水施設",
          labelOptions = labelOptions(
            noHide = F,
            direction = "bottom",
            style = lavel_temp,
            layerId = "上水施設"
          )
        ) %>%

        addCircleMarkers(
          lng = sluice_gate$lng,
          lat = sluice_gate$lat,
          #fillColor = "blue",
          radius = 5.,
          color = "blue",
          dashArray = "2",
          label = "取水口",
          group = "取水口",
          labelOptions = labelOptions(
            noHide = F,
            direction = "bottom",
            style = lavel_temp,
            layerId = "取水口"
         )) %>%

      addLayersControl(
        overlayGroups = c("給水人口", "上水施設", "取水口", "水道用井戸", "下水施設"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft"
      ) %>%
      
      # leaflet::addLegend(pal = pal_population, 
      #                    values = ~population$Jinkou, 
      #                    opacity = 0.7, 
      #                    title = "人口", 
      #                    position = "topleft") %>%
      
      leaflet::addLegend(pal = pal_ws_population, 
                         values = ~ws_population$P21A_004, 
                         opacity = 0.7, 
                         title = "給水人口(千人)",
                         position = "topleft") %>%
      hideGroup(c("給水人口", "上水施設", "取水口", "水道用井戸", "下水施設"))
  })
 
  # Supplymental example of Mapdeck 3D visualization(Not used)
  output$'3Dmap' <- renderMapdeck({
    mapdeck(
      style = mapdeck_style('dark'), pitch = 45) %>%
      add_line(
        data = df
        , layer_id = "line_layer"
        , origin = c("upLong", "upLat")
        , destination = c("dnLong", "dnLat")
        , stroke_colour = "upVal"
        , stroke_width = "upVal"
      )
  })
  
  # TAB 1 OBSERVE ---------------------
  # Source informations
  observeEvent(input$map_click, {
    print(input$map_click)
    #srclat <- reactive({ srclat <- input$map_click$lat; return(srclat); })
    #srclng <- reactive({ srclng <- input$map_click$lng; return(srclng); })
  })
  
  pointCount <- 0
  observe({
    if (is.null(mapNo()) | is.null(map())) {
      return(NULL)
    }
    

    lavel_temp <- list(
      "color" = "yellow",
      #"font-family" = "arial",
      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
      "font-size" = "20px",
      "border-color" = "rgba(0,0,0,0.5)",
      "background-color" = "black",
      "opacity" = 0.65)
    
    isolate({
      map <- map()
      mapNo <- mapNo()
      print(mapNo)
      
      if (mapNo == 3) {
        targetData <- wells
        #targetItem <- ~population$Jinkou
        label <- labels_wells
        XXX <- wells$X
        YYY <- wells$Y
        index <- 1:nrow(wells)
        ccolors <- "cyan"
        chrTitle   <- "水道用井戸"
      } else if (mapNo == 1) {
        targetData <- water_supply
        #targetItem <- ~population$Jinkou
        label <- labels_watersupply
        XXX <- water_supply$X
        YYY <- water_supply$Y
        index <- 1:nrow(water_supply)
        ccolors  <- "#ffa500"
        chrTitle   <- "上水施設"
      } else if (mapNo == 2) {
        targetData <- sewage_facility
        label <- labels_sewage_facility
        XXX <- sewage_facility$X
        YYY <- sewage_facility$Y
        index <- 1:nrow(sewage_facility)
        ccolors  <- "green"
        chrTitle   <- "下水施設"
      } else {
        return()
      }
    })
    
    pointCount <<- max(index)
    map
  })

  getIndexColor <- function(value, min, max) {
    if (is.na(value)) return(-1)
    if (value < min || value > max) return(0)
    parts <- 5
    index <- -1
    startValue <- min
    delta <- (max - min) / (parts - 1)
    while (startValue <= value) {
      index = index + 1
      startValue = startValue + delta
    }
    return(index)
  }

  getColor <- function(index) {
    if (index == -1) return('#808080')
    color <- rainbowPallet[index + 1]
    return(toString(color))
  }

  setLegends <- function () {
    map <- map()
    data <- data()

    if(input$mapselected == "Google Sattelite"){
      basemap <- "http://www.google.cn/maps/vt?lyrs=s@189&gl=cn&x={x}&y={y}&z={z}"
      til <- 'Maps by <a href="http://www.google.com/">Google</a>'    
    } else if (input$mapselected == "OpenStreetMap"){
      basemap <- "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
      til <- 'Maps by <a href="https://www.openstreetmap.org/">OSM</a>'    
    } else if (input$mapselected == "GSJ Standard"){
      basemap <- "https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png"
      til <- 'Maps by <a href="https://maps.gsi.go.jp/development/ichiran.html">GSJ</a>'
    } else if (input$mapselected == "GSJ Hillshade"){
      basemap <- "http://cyberjapandata.gsi.go.jp/xyz/hillshademap/{z}/{x}/{y}.png"
      til <- 'Maps by <a href="https://maps.gsi.go.jp/development/ichiran.html">GSJ</a>'
    } else if (input$mapselected == "Mapbox"){
      basemap <-"//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
      til <- 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    }

    # Set target data (River measuremet data e.g. discharge, concentration)
    ido = map$POINT_Y
    keido = map$POINT_X
    ondate <- as.Date(data$YMD)

    isolate({
      datafiltered <- data[which(ondate == input$currentDate), ]  #datafiltered <- data[which(ondate == as.Date("2008-01-01")), ]
      datafiltered[datafiltered$ITEM != input$variableselected, 5] <- NA

      # Create concentration data to be interpolated (filter data: map$ID_No > 0)
      dfc_up <- NULL; dfc_dn <- NULL; dfc <- NULL
      conc_keido <- map[map$ID_No > 0, 4]
      conc_ido   <- map[map$ID_No > 0, 5]
      conc_name  <- map[map$ID_No > 0, 1]
      conc <- datafiltered[datafiltered$ITEM == "汚染濃度", 5] 

      X_up <- as.numeric(conc_keido$POINT_X[1:nrow(conc_name) - 1])
      Y_up <- as.numeric(conc_ido$POINT_Y[1:nrow(conc_name) - 1])
      C_up <- conc[1:nrow(conc_name) - 1]
      dfc_up <- as.data.frame(cbind(X_up,Y_up,C_up))
      X_dn <- conc_keido$POINT_X[2:nrow(conc_name)]
      Y_dn <- conc_ido$POINT_Y[2:nrow(conc_name)]
      C_dn <- conc[2:nrow(conc_name)]
      dfc_dn <- as.data.frame(cbind(X_dn,Y_dn,C_dn))
      dfc <- as.data.frame(cbind(dfc_up,dfc_dn))
      colnames(dfc) <- c("upLong", "upLat", "upVal", "dnLong", "dnLat", "dnVal")
      
      df <- dfc
      setDT(df)
      
      ## create an 'id' / index value. This assumes each row of your data is a separate line. 
      df[, idx := .I]
      
      ## create an `sfc` column (where each row is an `sfg` object)
      sf <- df[  , {
        geometry <- sf::st_linestring(x = matrix(c(upLong, upLat, dnLong, dnLat), ncol = 2, byrow = T))
        geometry <- sf::st_sfc(geometry)
        geometry <- sf::st_sf(geometry = geometry)
      } , by = idx]
      
      ## convert to sf
      sf <- sf::st_as_sf(sf)

      # Link dataset
      map@data <- datafiltered
      map$variableplot <- as.numeric(map@data$VALUE) #[ ,input$variableselected])
      
      # Make color pallet & label
      avg_Val <- (as.numeric(df$upVal) + as.numeric(df$dnVal))/2
      min <- min(df$upVal)
      max <- max(df$upVal)
      if (is.na(min)) min = 0
      if (is.na(max)) max = 1
      pal_concRiver <- colorBin(rainbowPallet, domain = c(min:max), na.color = "#808080")
      min <- min(map$variableplot)
      max <- max(map$variableplot)
      if (is.na(min)) min = 0
      if (is.na(max)) max = 1
      pal_dichRiver <- colorBin("YlOrRd", domain = c(min:max))
      minValue <- min(map$variableplot, na.rm = TRUE)
      maxValue <- max(map$variableplot, na.rm = TRUE)

      labels_river <- sprintf("%s: %g", map@data$NAME, map$variableplot ) %>% lapply(htmltools::HTML)
      #labels_ConcRiver <- sprintf("%s %s: %g", df$idx,"濃度(mg/m3)",df$upVal) %>% lapply(htmltools::HTML)

      session$sendCustomMessage("clearCircleMarkers", '河川観測点')
      session$sendCustomMessage("onCheckLegend", '')

      lp <- leafletProxy(mapId = "map", data = map@data) %>%
        clearTiles() %>%
        removeControl(layerId = "Gage") %>%
        removeControl(layerId = "legend_discharge") %>%
        removeControl(layerId = "legend_concentration") %>%
        addTiles(basemap, attribution = til)
      
      for (index in 1:length(map@data$NAME)) {
        lp <- addCircleMarkers(
            lp,
            group = "河川観測点",
            lng = keido[index],
            lat = ido[index],
            data = map$variableplot[index],
            fillColor = getColor(getIndexColor(map$variableplot[index], minValue, maxValue)),
            #radius = ~sqrt(map$variableplot),
            color = "white",
            dashArray = "2",
            fillOpacity = 0.7,
            label = sprintf("%s: %g", map@data$NAME[index], map$variableplot[index] ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
              group = "河川観測点",
              noHide = F,
              direction = "bottom",
              style = list(
              "color" = "yellow",
              "font-family" = "arial",
              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
              "font-size" = "25px",
              "border-color" = "rgba(0,0,0,0.5)",
              "background-color" = "black",
              "opacity" = 0.65
            ),
            layerId = map@data$NAME[index]
          )
        )
      }

      lp %>%
        # River polylines (remove, use Hotline in client-side)
        # addPolylines(data = sf,
        #              color = pal_concRiver(df$upVal),
        #              weight = 12) %>%
        addLegend(
          pal = pal_dichRiver, 
          values = ~variableplot, 
          opacity = 0.7, 
          title = "河川流量(m3/s)", #input$variableselected, 
          position = "topleft", 
          layerId = "legend_discharge"
        ) %>% 
        addLegend(
          pal = pal_concRiver, 
          values = ~variableplot, 
          opacity = 0.7, 
          title = "汚染濃度(mg/m3)", #input$variableselected, 
          position = "topleft", 
          layerId = "legend_concentration"
        ) %>%
        fitBounds(~min(keido), ~min(ido), ~max(keido), ~max(ido))
    })
  }
  
  observe({
      # Reactive display of river discharge & concentration
      map <- map()
      data <- data()

      if (is.null(map()) | is.null(data())) {
        return(NULL)
      }

      if (input$mapselected == "Google Sattelite") {
        basemap <- "http://www.google.cn/maps/vt?lyrs=s@189&gl=cn&x={x}&y={y}&z={z}"
        til <- 'Maps by <a href="http://www.google.com/">Google</a>'    
      } else if (input$mapselected == "OpenStreetMap") {
        basemap <- "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
        til <- 'Maps by <a href="https://www.openstreetmap.org/">OSM</a>'    
      } else if (input$mapselected == "GSJ Standard") {
        basemap <- "https://cyberjapandata.gsi.go.jp/xyz/std/{z}/{x}/{y}.png"
        til <- 'Maps by <a href="https://maps.gsi.go.jp/development/ichiran.html">GSJ</a>'
      } else if (input$mapselected == "GSJ Hillshade") {
        basemap <- "http://cyberjapandata.gsi.go.jp/xyz/hillshademap/{z}/{x}/{y}.png"
        til <- 'Maps by <a href="https://maps.gsi.go.jp/development/ichiran.html">GSJ</a>'
      } else if (input$mapselected == "Mapbox") {
        basemap <- "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
        til <- 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      }
      
     setLegends()
  })
  
  observe({
    # Reactive display Radius Of Influence (ROI)
    if (is.null(srclat()) | is.null(srclng()) | is.null(srcROI()) | is.null(map())) {
      return(NULL)
    }

    isolate({
      map <- map()
      srclat <- srclat()
      srclng <- srclng()
      srcROI <- srcROI()
      leafletProxy(mapId = "map") %>%
      removeControl(layerId = "srclatlng") %>%
      removeControl(layerId = "srcROI") %>%
      addCircles(
        lng = srclng, 
        lat = srclat,
        #fillColor = ~pal_river(variableplot),
        radius = 10.0,
        color = "red",
        dashArray = "2",
        fillOpacity = 1.0,
        layerId = "srclatlng"
      ) %>%  
      addCircles(
        lng = srclng,
        lat = srclat,
        fillColor = F,
        radius = srcROI,
        color = "red",
        dashArray = "2",
        fillOpacity = 0.3,
        layerId = "srcROI"
      )
    })
  })

  getHotlineOptions <- function () {
    options <- list(
      "weight" = hotlineWeight,
      "outlineWidth" = hotlineOutlineWidth,
      "outlineColor" = hotlineOutlineColor,
      "smoothFactor" = hotlineSmooth,
      "rainbowPallet" = rainbowPallet
    )
    return(options)
  }

  session$sendCustomMessage("onUpdateHotlineOptions", getHotlineOptions())

  updateVariableSelected <- function () {
    if (input$variableselected == '汚染濃度') {
      updateSelectInput(session, 'variableselected', selected = '河川流量')
      updateSelectInput(session, 'variableselected', selected = '汚染濃度')
    }
  }

  setHotLine <- function () {
    map <- map()
    data <- data()

    # 1. Get Location array
    locations <- data.frame(map)[, c("ID_No", "Name", "POINT_X", "POINT_Y")]
    colnames(locations) <- c("ID_NO", "NAME", "POINT_X", "POINT_Y")

    # 2. Get Item value by Date
    ondate <- as.Date(data$YMD)
    datafiltered <- data[which(ondate == input$currentDate), ]
    datafiltered[datafiltered$ITEM != input$variableselected, 5] <- NA

    # 3. Merge 2 data frame
    hotlineData <- merge(datafiltered, locations, by = "NAME")
    hotlineData <- hotlineData[order(hotlineData$ID_NO), ]
    hotlineData <- hotlineData[, c("NAME", "POINT_X", "POINT_Y", "VALUE")]
    
    # 4. Get Min-Max value in whole Timeseries
    summary <- summaryByItem(input$variableselected)
    
    # 5. Build Hotline options
    options <- getHotlineOptions()

    # 5. Send client new Hotline data
    result = list(hotlineData = hotlineData, summary = summary, options = options)

    session$sendCustomMessage("onUpdateHotlineOptions", options)
    datafiltered <- data[which(ondate == input$currentDate), ]
    datafiltered[datafiltered$ITEM != input$variableselected, 5] <- NA

    session$sendCustomMessage('onUpdateColorMarkers', datafiltered)
    updateVariableSelected()
  }
  
  #' Hotline Data change events
  observe({
    # Anything below change will mark as Hotline data changed
    map <- map() 
    data <- data()
    input$currentDate

    if (is.null(map()) | is.null(data())) {
      return(NULL)
    }
    
    isolate({
      setHotLine()
    })
  })

  # TAB 2 ##################################################
  
  output$ts_Q <- renderPlotly({
    data <- data()
    dataxts <- NULL
    obs <- unique(data$NAME)
    for(l in 1:length(obs)){
      datastation <- data[data$NAME == obs[l],]
      dayaxis <- as.Date(datastation$YMD)
      dd <- xts(datastation[datastation$ITEM == "河川流量",5], dayaxis, frequency = 365.25)
      dataxts <- cbind(dataxts, dd)
    }
    colnames(dataxts) <- as.character(obs[1:7])
    dataxts <- as.data.frame(dataxts) #important, Akutagawa,Yoshidabashi,Takahama,Oobe,Kamitodoromi,Hirakata,Tokura
    
    plot_ly(x = ~dayaxis,y = ~dataxts[,1], 
                type = "scatter",
                mode = "lines",
                name = "芥川",
                text = paste("芥川",dayaxis, ":",1)) %>% 
      add_trace(x = ~dayaxis,y = dataxts[,2],
                type = "scatter",
                mode = "lines", 
                name = "吉田橋上流",
                #yaxis= "y2",
                text = paste("吉田橋上流", dayaxis, ":",2)) %>%
      add_trace(x = ~dayaxis,y = dataxts[,3],
                type = "scatter",
                mode = "lines", 
                name = "高浜",
                #yaxis= "y2",
                text = paste("高浜", dayaxis, ":",2)) %>%
      add_trace(x = ~dayaxis,y = dataxts[,4],
                type = "scatter",
                mode = "lines", 
                name = "小戸",
                #yaxis= "y2",
                text = paste("小戸", dayaxis, ":",2)) %>%
      add_trace(x = ~dayaxis,y = dataxts[,5],
                type = "scatter",
                mode = "lines", 
                name = "上止々呂美",
                #yaxis= "y2",
                text = paste("上止々呂美", dayaxis, ":",2)) %>%
      add_trace(x = ~dayaxis,y = dataxts[,6],
                type = "scatter",
                mode = "lines", 
                name = "枚方",
                #yaxis= "y2",
                text = paste("枚方", dayaxis, ":",2)) %>%
      add_trace(x = ~dayaxis,y = dataxts[,7],
                type = "scatter",
                mode = "lines", 
                name = "利倉",
                #yaxis= "y2",
                text = paste("利倉", dayaxis, ":",2)) %>%
      layout(title = "River flow",
             xaxis = list(showgrid = T,title = "Date"),
             yaxis = list(showgrid = F,title = "Discharge(m3/s)",side="left"),
             yaxis2= list(title="Rainfall(mm)",
                          overlaying = "y", 
                          side="right",
                          ticks="outside"),
             yaxis = list(type = "log"),
             showlegend = TRUE)
    
  })

  output$boxp_Q <- renderPlotly({
    #data<-read.csv("YDR_basin.csv")
    data <- data()
    dataxts <- NULL
    obs <- unique(data$NAME)
    for(l in 1:length(obs[1:7])){
      datastation <- data[data$NAME == obs[l],]
      dayaxis <- as.Date(datastation$YMD)
      dd <- datastation[datastation$ITEM == "河川流量", datastation[,5]]
      dataxts <- cbind(dataxts, dd)
    }
    
    colnames(dataxts) <- as.character(obs[1:7])
    dataxts <- as.data.frame(dataxts) #important, Akutagawa,Yoshidabashi,Takahama,Oobe,Kamitodoromi,Hirakata,Tokura
    
    plot_ly(alpha = 0.8, type ="box") %>%
        add_trace(x = dataxts[,1], name="芥川") %>%
        add_trace(x = dataxts[,2], name="吉田橋上流") %>%
        add_trace(x = dataxts[,3], name="高浜") %>%
        add_trace(x = dataxts[,4], name="小戸") %>%
        add_trace(x = dataxts[,5], name="上止々呂美") %>%
        add_trace(x = dataxts[,6], name="枚方") %>%
        add_trace(x = dataxts[,7], name="利倉") %>% 
        layout(
          title = "Discharge", 
          xaxis = list(title = "Cubic meter in second")
        )
  })
  
  # TAB 3 ##################################################
  output$table <- renderDT(data())


  # TAB 4 ##################################################
  # Exact solution of contaminant transport for river spill
  # // parameter setting //
  distance <- reactive({
    d <- as.numeric(input$dist)
    return(d)
  })
  maxtime <- reactive({
    tm <- as.numeric(input$tmax)
    return(tm)
  })
  releasedmass <- reactive({
    m <- as.numeric(input$mass)
    return(m)
  })
  sectionarea <- reactive({
    a <- as.numeric(input$area)
    return(a)
  })
  dispersion <- reactive({
    dc <- as.numeric(input$disp)
    return(dc)
  })
  rivervelocity <- reactive({
    v <- as.numeric(input$velo)
    return(v)
  })
  skewness <- reactive({
    g <- as.numeric(input$gamm)
    return(g)
  })

  distOptions <- reactive({
    distOptions <- as.numeric(input$distOptions)
    return(distOptions)
  })
#  maxtOptions <- reactive({
#    maxtOptions <- as.numeric(input$maxtOptions)
#    return(maxtOptions)
#  })
  massOptions <- reactive({
    massOptions <- as.numeric(input$massOptions)
    return(massOptions)
  })
  areaOptions <- reactive({
    areaOptions <- as.numeric(input$areaOptions)
    return(areaOptions)
  })
  dispOptions <- reactive({
    dispOptions <- as.numeric(input$dispOptions)
    return(dispOptions)
  })

  veloOptions <- reactive({
    veloOptions <- as.numeric(input$veloOptions)
    return(veloOptions)
  })

  gammOptions <- reactive({
    gammOptions <- as.numeric(input$gammOptions)
    return(gammOptions)
  })

  getSolc <- function(sec2day, dist, maxt, mass, area, disp, velo, gamm) {
    solc <- array(0:as.integer(maxt / sec2day), dim = c(1))
    tim <- seq(0, maxt, 86400)

    k = 1
    for(l in tim) {
      mbya <- mass / area
      val1 <- mbya / sqrt(4 * pi * disp * l)
      val2 <- -1.0 * (dist - velo * l) * (dist - velo * l) / (4 * disp * l)
      hfnc <- hermite(3, kind = "h")
      z    <- (dist - velo * l) / sqrt(2 * disp * l)
      vtmp <- data.frame(1, as.function(hfnc)(z))
      val3 <- 1 - (gamm * vtmp)
      solc[k] <- val1 * exp(val2) * val3[1, 1]
      k <- k + 1  
    }
    return(solc)
  }

  getDataBreakthrough <- function () {
    sec2day <- 86400.
    
    dist <- distance()
    maxt <- maxtime() * sec2day
    mass <- releasedmass()
    area <- sectionarea()
    disp <- dispersion() #* sec2day
    velo <- rivervelocity() #* sec2day
    gamm <- skewness()

    distOptions <- distOptions()  
#    maxtOptions <- maxtOptions()  
    massOptions <- massOptions()  
    areaOptions <- areaOptions()
    dispOptions <- dispOptions()
    veloOptions <- veloOptions()
    gammOptions <- gammOptions()

    distLength <- length(distOptions)
#    maxtLength <- length(maxtOptions)
    massLength <- length(massOptions)
    areaLength <- length(areaOptions)
    dispLength <- length(dispOptions)
    veloLength <- length(veloOptions)
    gammLength <- length(gammOptions)

    # unit: s
    tim <- seq(0, maxt, 86400)
    # unit: kg/m3

    currentSolc = list()
    currentSolcValue <- getSolc(sec2day, dist, maxt, mass, area, disp, velo, gamm)
    currentSolcName <- sprintf('L%s_M%s_A%s_D%s_V%s_G%s', dist, mass, area, disp, velo, gamm)
    currentSolc[[currentSolcName]] = currentSolcValue

    solcList <- list()
    solcList$x = tim / sec2day

    getIndex <- function (distIndex=1, massIndex=1, areaIndex=1, dispIndex=1, veloIndex=1, gammIndex=1) {
      loop0 <- gammIndex - 1
      loop1 <- loop0 * veloLength  + (veloIndex - 1)
      loop2 <- loop1 * dispLength + (dispIndex - 1)
      loop3 <- loop2 * areaLength + (areaIndex - 1)
      loop4 <- loop3 * massLength + (massIndex - 1)
#      loop5 <- loop4 * maxtLength + (maxtIndex - 1)
      loop5 <- loop4 * distLength + (distIndex - 1)
      index <- loop5 + 1
      return(index)
    }

    
    for (di in 1:distLength) {
      newDist <- ifelse(distLength <= 1, dist, distOptions[di])
        for (m in 1:massLength) {
          newMass <- ifelse(massLength <= 1, mass, massOptions[m])
          for (a in 1:areaLength) {
            newArea <- ifelse(areaLength <= 1, area, areaOptions[a])
            for (d in 1:dispLength) {
              newDisp <- ifelse(dispLength <= 1, disp, dispOptions[d])
              for (v in 1:veloLength) {
                newVelo <- ifelse(veloLength <= 1, velo, veloOptions[v])
                for (g in 1:gammLength) {
                  newGamm <- ifelse(gammLength <= 1, gamm, gammOptions[v])
                  newSolc <- getSolc(sec2day, newDist, maxt, newMass, newArea, newDisp, newVelo, newGamm)
                  # index <- getIndex(m, a, d, v, g)
                  # solcList[[index]] <- newSolc
                  newCol <- sprintf('L%s_M%s_A%s_D%s_V%s_G%s', newDist, newMass, newArea, newDisp, newVelo, newGamm)
                  solcList[[newCol]] <- newSolc
                  if (gammLength <= 1) { break }
                }
                if (veloLength <= 1) { break }
              }
              if (dispLength <= 1) { break }
            }
            if (areaLength <= 1) { break }
          }
          if (massLength <= 1) { break }
        }
      if (distLength <= 1) { break }
    }

    results = list(tim = tim, sec2day = sec2day, currentSolc = currentSolc, solcList = solcList)

    return(results)
  }

  getDefaultPlot <- function () {
    p <- renderPlotly({
      data <- getDataBreakthrough()
      name <- names(data$currentSolc)[1]

      plot_ly(
        y = data$currentSolc[[name]],
        name = name,
        text = name,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = "Estimated Breakthrough Curve",
        xaxis = list(showgrid = T, title = "経過時間(日)"),
        yaxis = list(showgrid = F, title = "物質濃度(mg/L)", side = "left"),
        showlegend = TRUE
      )
    })
    return(p)
  }

  output$solve <- getDefaultPlot()

  observe({
    dist <- distance()
#    maxt <- maxtime()
    mass <- releasedmass()
    area <- sectionarea()
    disp <- dispersion() #* sec2day
    velo <- rivervelocity() #* sec2day
    gamm <- skewness()

    distOptions <- distOptions()  
#    maxtOptions <- maxtOptions()  
    massOptions <- massOptions()  
    areaOptions <- areaOptions()
    dispOptions <- dispOptions()
    veloOptions <- veloOptions()
    gammOptions <- gammOptions()

    output$solve <- getDefaultPlot()
  })

  # Reset data
  observeEvent(input$refresh, {
    updateSelectInput(session, 'dist', choices = c(10000.))
    updateTextInput(session, 'distOptions', '')
#    updateSelectInput(session, 'maxt', choices = c(5000.))
#    updateTextInput(session, 'maxtOptions', '')
    updateSelectInput(session, 'mass', choices = c(5000.))
    updateTextInput(session, 'massOptions', '')
    updateSelectInput(session, 'area', choices = c(1500.))
    updateTextInput(session, 'areaOptions', '')
    updateSelectInput(session, 'disp', choices = c(1000.))
    updateTextInput(session, 'dispOptions', '')
    updateSelectInput(session, 'velo', choices = c(0.15))
    updateTextInput(session, 'veloOptions', '')
    updateSelectInput(session, 'gamm', choices = c(0.00))
    updateTextInput(session, 'gammOptions', '')
    output$solve <- getDefaultPlot()
  })

  observeEvent(input$execute, {
    data <- getDataBreakthrough()
    plotLength <- length(data$solcList) - 1
    if (plotLength <= 1) {
      output$solve <- getDefaultPlot()
    } else {
      output$solve <- renderPlotly({
        p <- plot_ly()
        for (i in 1:plotLength) {
          name <- names(data$solcList)[i + 1]
          p <- add_trace(
            p,
            y = data$solcList[[name]],
            name = name,
            text = name,
            type = "scatter",
            mode = "lines"
          )
        }
        p %>%
        layout(
          title = "Estimated Breakthrough Curve",
          xaxis = list(showgrid = T, title = "経過時間(日)"),
          yaxis = list(showgrid = F, title = "物質濃度(mg/L)", side = "left"),
          showlegend = TRUE
        )
        return(p)
      })
    }
  })

  output$downloadData = downloadHandler(
    filename = "breakthrough.csv",
    content = function(file) {
      data <- getDataBreakthrough()
      name <- names(data$solcList)[2]
      output <- data.frame(
        x = data$tim / data$sec2day,
        y = data$solcList[[name]]
      )
      write.csv(output, file)
    }
  )

  output$downloadAll = downloadHandler(
    filename = "breakthrough-all.csv",
    content = function(file) {
      data <- getDataBreakthrough()
      output <- data$solcList
      write.csv(output, file)
    }
  )

  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

  # MODAL
  modal <- function (inputId = '', initialValue = 0, range) {
    modalDialog(
      class = inputId,
      fluidRow(
        column(
          4,
          numericInput(
            inputId = "minDistance",
            label = "下限",
            value = ifelse(length(range) > 1, range[1], initialValue),
            min = 0.0,
            step = 1.0,
          )
        ),
        column(
          4,
          numericInput(
            inputId = "maxDistance",
            label = "上限",
            value = ifelse(length(range) > 1, range[length(range)], initialValue),
            min = 0.0,
            step = 1.0
          )
        ),
        column(
          4,
          numericInput(
            inputId = "divisonDistance",
            label = "分割数",
            value = ifelse(length(range) > 1, length(range) - 1, 1),
            min = 0.0,
            step = 1.0
          )
        ),
        column(
          12,
          class="options",
          tags$table(
            class="table table-bordered",
            tags$thead(
              tags$tr(
                tags$th(align = "center", strong('Case')),
                tags$th(align = "center", strong('Value'))
              )
            ),
            tags$tbody()
          )
        )
      ),
      footer = tagList(
        actionButton("ok", "OK", class="btn-success"),
        modalButton("閉じる")
      )
    )
  }

  observeEvent(input$openDistanceModal, {
    showModal(modal('dist', 5000., as.numeric(input$distOptions)))
  })
#  observeEvent(input$openTimeModal, {
#    showModal(modal('maxt', 5000., as.numeric(input$maxtOptions)))
#  })
  observeEvent(input$openLeakageModal, {
    showModal(modal('mass', 5000., as.numeric(input$massOptions)))
  })

  observeEvent(input$openAreaRiverModal, {
    showModal(modal('area', 1500., as.numeric(input$areaOptions)))
  })

  observeEvent(input$openDispersionModal, {
    showModal(modal('disp', 1000., as.numeric(input$dispOptions)))
  })

  observeEvent(input$openFlowModal, {
    showModal(modal('velo', 0.15, as.numeric(input$veloOptions)))
  })

  observeEvent(input$openSkewnessModal, {
    showModal(modal('gamm', 0.00, as.numeric(input$gammOptions)))
  })

  observeEvent(input$ok, {
    modalName <- input$modalName
    nameInputOptions <- paste0(modalName, 'Options')
    options <- input[[nameInputOptions]]
    updateSelectInput(session, modalName, choices = options)
  })


  # TAB 5: Hotline Settings #############################################

  updateColor <- function (colors) {
    updateColourInput(session, 'color1', '', colors[1])
    updateColourInput(session, 'color2', '', colors[2])
    updateColourInput(session, 'color3', '', colors[3])
    updateColourInput(session, 'color4', '', colors[4])
    updateColourInput(session, 'color5', '', colors[5])
  }

  updateInputValue <- function () {
    updateNumericInput(session, 'bufferDistance', value = bufferDistance)
    updateNumericInput(session, 'hotlineOutlineWidth', value = hotlineOutlineWidth)
    updateNumericInput(session, 'hotlineWeight', value = hotlineWeight)
    updateNumericInput(session, 'hotlineSmooth', value = hotlineSmooth)
    updateTextInput(session, 'hotlineOutlineColor', value = hotlineOutlineColor)
    updateTextInput(session, 'rainbowPalletSelected', value = rainbowPallet)
    updateColor(rainbowPallet)
  }
  updateInputValue()

  observeEvent(input$saveHotlineSettings, saveSettings())
  observeEvent(input$saveDistance, saveSettings())

  saveSettings <- function () {
    color1 <- input$color1
    color2 <- input$color2
    color3 <- input$color3
    color4 <- input$color4
    color5 <- input$color5

    outputSettings <- list()
    outputSettings$hotlineOutlineWidth <- as.numeric(input$hotlineOutlineWidth)
    outputSettings$hotlineWeight <- as.numeric(input$hotlineWeight)
    outputSettings$hotlineSmooth <- as.numeric(input$hotlineSmooth)
    outputSettings$hotlineOutlineColor <- as.character(input$hotlineOutlineColor)
    outputSettings$rainbowPallet <- c(color1, color2, color3, color4, color5)
    outputSettings$bufferDistance <- as.numeric(input$bufferDistance)

    projectSettings <- toJSON(outputSettings)
    write(projectSettings, file="data/settings.json")
    setHotlineConfig()
    updateInputValue()
    setHotLine()
    setLegends()
  }

  observe({
    rainbowPalletSelected <- rainbowPalletSelected()
    updateColor(rainbowPalletSelected)
  })

  getResults <- function(dates, dateInput, step) {
    start <- as.Date(dateInput)
    dates <- as.Date(dates)
    dates <- unique(dates)
    minDate <- min(dates)
    maxDate <- max(dates)
    if (start < minDate || start > maxDate) {
      start = minDate
    }
    results <- list(
      dates = dates,
      start = start,
      min = minDate,
      max = maxDate,
      step = step
    )
    return(results)
  }

  setHotLineRiver <- function () {
    map <- map()
    data <- data()

    if (is.null(data) | is.null(map)) {
      return(NULL)
    }

    onDate <- as.Date(data$YMD)
    targetDate <- input$currentDate
    dataFiltered <- data[which(onDate == targetDate), ]
    dataFiltered[dataFiltered$ITEM != input$variableselected, 5] <- NA
    minValue <- min(dataFiltered$VALUE, na.rm =TRUE)
    maxValue <- max(dataFiltered$VALUE, na.rm =TRUE)
    dataFilteredWithoutNA <- dataFiltered %>% na.exclude

    mapPoints <- cbind(map$POINT_X, map$POINT_Y, dataFiltered$VALUE, dataFiltered$ITEM)
    results <- list(mapPoints = mapPoints, min = minValue, max = maxValue, bufferDistance = bufferDistance)
    session$sendCustomMessage("onChangeDateRiver", results)
  }

  observe({
    input$currentDate
    input$variableselected
    setHotLineRiver()
  })

  observe({
    data <- data()
    if (is.null(data) | is.null(map())) {
      return(NULL)
    }
    variableselected <- input$variableselected
    isolate({
      onDate <- as.Date(data$YMD)
      datafiltered <- data[which(onDate == input$currentDate), ]
      datafiltered[datafiltered$ITEM != variableselected, 5] <- NA
      session$sendCustomMessage('onUpdateColorMarkers', datafiltered)
    })
  })

  observeEvent(input$runAnimationRiver, {
    data <- data()
    results <- getResults(data$YMD, input$currentDate, 5)
    session$sendCustomMessage("runAnimationRiver", results)
  })
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}
