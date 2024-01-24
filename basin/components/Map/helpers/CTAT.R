pacman::p_load(readxl,xts)

# Build leaflet map
buildMap <- function (mapData=list(), refresh=FALSE) {
  LMap <- leaflet(mapData) %>%
    addTiles(basemap, attribution = til) %>%  
    addMiniMap(toggleDisplay = TRUE, position = "bottomleft",minimized = T) %>%
    leaflet::addScaleBar(position = "bottomright") %>%
    addPolygons(
      data = ws_population, group="給水人口", fillColor = ~pal_ws_population(ws_population$P21A_004r), stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5
    ) %>%
    addCircleMarkers(
      lng = ~wells$X, lat = ~wells$Y, radius = 5., color = "cyan", dashArray = "2", label = labels_wells, group = "水道用井戸",
      labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "水道用井戸")
    ) %>%
    addCircleMarkers(
      lng = ~sewage_facility$X, lat = ~sewage_facility$Y,radius = 5., color = "green", dashArray = "2", label = labels_sewage_facility, group = "下水施設",
      labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "下水施設")
    ) %>%
    addCircleMarkers(
      lng = water_supply$X, lat = water_supply$Y,radius = 5., color = "#ffa500", dashArray = "2", label = labels_watersupply, group = "上水施設",
      labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "上水施設")
    ) %>%
    addCircleMarkers(
      lng = sluice_gate$lng, lat = sluice_gate$lat,radius = 5., color = "blue", dashArray = "2", label = "取水口", group = "取水口",
      labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "取水口")
    ) %>%
    # Circles for PRTR stations
    addCircleMarkers(
      lng = prtr_station_old$lng, lat = prtr_station_old$lat,layerId=~prtr_station_old$ID,radius = 5., color = "plum", dashArray = "2",
      label = paste(prtr_station_old$company.name1, prtr_station_old$address1,prtr_station_old$address2,prtr_station_old$address3) %>% lapply(htmltools::HTML), group = "PRTR事業所➔河川",
      labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES)
    )  %>%
    addCircleMarkers(
      lng = prtr_station1$lng, lat = prtr_station1$lat,layerId=~prtr_station1$ID,radius = 5., color = "purple", dashArray = "2",
      label = paste(prtr_station1$company.name1, prtr_station1$address1,prtr_station1$address2,prtr_station1$address3) %>% lapply(htmltools::HTML), group = "PRTR事業所➔河川",
      labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES)
    )  %>%
    addCircleMarkers(
      lng = prtr_station2$lng, lat = prtr_station2$lat,layerId=~prtr_station2$ID,radius = 5., color = "purple", dashArray = "2",
      label = paste(prtr_station2$company.name1, prtr_station2$address1,prtr_station2$address2,prtr_station2$address3) %>% lapply(htmltools::HTML), group = "PRTR事業所➔河川",
      labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES)
    )  %>%
    
    # add polyline layer for clicking river/lake and showing prtr stations associated to it
    addPolygons(
      group="河川➔PRTR事業所", data=prtr_lake,layerId=~prtr_lake$LinkID,color="royalblue",weight=1,fillOpacity = 0.7
    ) %>%
    addPolylines(
      group="河川➔PRTR事業所",data = prtr_river_old,layerId=~prtr_river_old$LinkID,color = "lightskyblue",opacity = 0.7,weight = "6"
    ) %>%
    addPolylines(
      group="河川➔PRTR事業所",data = prtr_river1,layerId=~prtr_river1$LinkID,color = "royalblue",opacity = 0.7,weight = "6"
    ) %>%
    addPolylines(
      group="河川➔PRTR事業所",data = prtr_river2,layerId=~prtr_river2$W05_004, color = "royalblue",opacity = 0.7,weight = "6"
    ) %>%
    # add simulation results
    addPolygons(
      group="大気シミュレーション", lng = coo1[,1], lat = coo1[,2], fillColor = "blue", fillOpacity = 0.5, weight = 0, label = "AEGL1"
    ) %>%
    addPolygons(
      group="大気シミュレーション", lng = coo2[,1], lat = coo2[,2], fillColor = "yellow", fillOpacity = 0.5, weight = 0, label = "AEGL2"
    ) %>%
    addPolygons(
      group="大気シミュレーション", lng = coo3[,1], lat = coo3[,2], fillColor = "red", fillOpacity = 0.5, weight = 0, label = "AEGL3"
    ) %>%
    addPolygons(
      group="河川シミュレーション", weight = 0, color = NULL, fillColor = "blue", fillOpacity = 0.8, data = st_zm(st_geometry(layer2))
    ) %>%
    addPolygons(
      group="河川シミュレーション", weight = 0, color = NULL, fillColor = "cyan", fillOpacity = 0.8, data = st_zm(st_geometry(layer3))
    ) %>%
    addPolygons(
      group="河川シミュレーション", weight = 0, color = NULL, fillColor = "#00ff00", fillOpacity = 0.8, data = st_zm(st_geometry(layer4))
    ) %>%
    addLayersControl(
      overlayGroups = c("給水人口", "上水施設", "取水口", "水道用井戸", "下水施設","PRTR事業所➔河川","河川➔PRTR事業所","大気シミュレーション","河川シミュレーション"),
      options = layersControlOptions(collapsed = FALSE), position = "topleft"
    ) %>%
    leaflet::addLegend(
      pal = pal_ws_population, values = ~ws_population$P21A_004, opacity = 0.7, title = "給水人口(千人)", position = "topleft"
    ) %>%
    hideGroup(c("給水人口", "上水施設", "取水口", "水道用井戸", "下水施設","PRTR事業所➔河川","河川➔PRTR事業所","大気シミュレーション","河川シミュレーション"))
  if (refresh) {
    output$map <- renderLeaflet(LMap)
  }
  return(LMap)
}

getHotlineSettings <- function () {
    return(list("weight" = hotlineWeight, "outlineWidth" = hotlineOutlineWidth, "outlineColor" = hotlineOutlineColor, "smoothFactor" = hotlineSmooth, "rainbowPallet" = rainbowPallet))
}

readParam <- function (datapath) {
  isCsv <- grepl('.csv$', datapath)
  if (isCsv) {
    tryCatch({
      dataParam <- fread(datapath) %>% as.data.frame
      keys <- names(dataParam)
      if (is.element('value', keys) & is.element('param_name', keys)) {
        dataParam$VALUE <- as.numeric(dataParam$value)
        dataParam$PARAM_NAME <- as.character(dataParam$param_name)
        dataParam %<>% select(PARAM_NAME,VALUE)
        return(dataParam)
      }
    }, error = function(e) {})
  }
  reset('csvfile2')
  return(NULL)
}

logA <- function(x) {
  if (min(x,na.rm=T)==0) x %<>% add(1) 
  return(log10(x))
}
nrmlz <- function(x) return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))

readDataLocation <- function (datapath) {
  isCsv <- grepl('.csv$', datapath)
  if (isCsv) {
    tryCatch({
      dataLocation <- fread(datapath, encoding = 'UTF-8', dec = ",")
      keys <- names(dataLocation)
      if (is.element('VALUE', keys) & is.element('YMD', keys)) {
        dataLocation$VALUE <- as.numeric(dataLocation$VALUE)
        dataLocation$LOG <- as.numeric(dataLocation$VALUE) %>% logA # log10 scale, adjusted with min value
        dataLocation$STN <- as.numeric(dataLocation$VALUE) %>% scale # standardized (mean 0, sd 1)
        dataLocation$NRM <- as.numeric(dataLocation$VALUE) %>% nrmlz # normalized (between 0 and 1)
        dataLocation$YMD <- as.Date(dataLocation$YMD)
        return(dataLocation)
      }
    }, error = function(e) {})
  }
  reset('csvfile')
  return(NULL)
}

readShapefiles <- function (shpdf, isRiverFile, session) {
    # Make empty list of geojson everytime import shapefile
    geojsonList <<- list()

    # Get folder name of shapefile
    tmpdirname <- dirname(shpdf$datapath[1])

    for (i in 1:nrow(shpdf)) {
        isJson <- grepl("\\.json$", shpdf$name[i])
        isGeojson <- grepl('\\.geojson$', shpdf$name[i])
        isCsv <- grepl("\\.csv$", shpdf$name[i])

        if (isJson) {
            # Read .json file, using javascript to add to leaflet map
            tryCatch({
                topoJsonData <- geojsonio::geojson_read(shpdf$datapath[i])
                session$sendCustomMessage('onAddJson', topoJsonData)
            }, error = function (err) {
                print(paste('Error read json file: ', err))
            })
        } else if (isGeojson) {
            # read geojson file, add to leaflet map later
            tryCatch({
                geoJsonData <- readOGR(shpdf$datapath[i])

                # Update global draw polygon if not river file
                if (isRiverFile == FALSE) {
                # geojsonList[[length(geojsonList) + 1]] <<- geoJsonData
                  geojsonList <<- geoJsonData
                }
                return(geoJsonData)
            }, error = function (err) {
                print(paste('Error read geojson file: ', err))
            })
        } else if (isCsv) {
            # Read csv file
            csvData <- read.csv(shpdf$datapath[i])
            sfData <- st_as_sf(csvData, coords = c("POINT_X", "POINT_Y"), remove = FALSE, crs = st_crs(4326))
            tempFileName <- paste(tempdir(), "/", shpdf$name[i], ".shp", sep="")
            st_write(sfData, tempFileName, driver="ESRI Shapefile", delete_dsn=TRUE, delete_layer=TRUE)
            shpData <- readOGR(tempFileName)
            return(shpData)
        } else {
            # By default filename will be 1, 2, 3....
            # Change filename to orginal name
            file.rename(shpdf$datapath[i], paste0(tmpdirname, "/", shpdf$name[i]))
        }
    }

    # Read .shp file
    shpdfName <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
    if (identical(shpdfName, character(0))) {
        return(NULL)
    } else {
        map <- readOGR(paste(tmpdirname, shpdfName, sep = "/"))
        return(map)
    }
}

updateCircleMarkers <- function () {
    map <- map()
    data <- data()

    # Set target data (River measurement data e.g. discharge, concentration)
    ido = map$POINT_Y
    keido = map$POINT_X
    ondate <- as.Date(data$YMD)

    isolate({
        datafiltered <- data[which(ondate == input$currentDate), ]  #datafiltered <- data[which(ondate == as.Date("2008-01-01")), ]
        datafiltered[datafiltered$ITEM != input$variableselected, 5] <- NA
        # Create concentration data to be interpolated (filter data: map$ID_No > 0)
        dfc_up <- NULL; dfc_dn <- NULL; dfc <- NULL #
        conc_keido <- map[(map$ID_No %>% as.numeric) > 0, "POINT_X"] 
        conc_ido   <- map[(map$ID_No %>% as.numeric) > 0, "POINT_Y"]
        conc_name  <- map[(map$ID_No %>% as.numeric) > 0, "Name"]
        conc <- datafiltered[datafiltered$ITEM == "汚染濃度", "VALUE"] 
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
        sf <- df[,{
                geometry <- sf::st_linestring(x = matrix(c(upLong, upLat, dnLong, dnLat), ncol = 2, byrow = T))
                geometry <- sf::st_sfc(geometry)
                geometry <- sf::st_sf(geometry = geometry)
            }, by = idx
        ]
        ## convert to sf
        sf <- sf::st_as_sf(sf)

        # Link dataset
        map@data <- datafiltered
        map$variableplot <- as.numeric(map@data$VALUE) 

        # Make color pallet & label
        avg_Val <- (as.numeric(df$upVal) + as.numeric(df$dnVal))/2
        min <- min(df$upVal)
        max <- max(df$upVal)
        if (is.na(min)) min = 0
        if (is.na(max)) max = 1
        pal_concRiver <- colorBin(rainbowPallet, domain = c(min:max), na.color = "#808080")
        min <- min(map$variableplot)
        max <- max(map$variableplot)
        if (is.na(min) || is.infinite(min)) min = 0
        if (is.na(max) || is.infinite(max)) max = 1
        pal_dichRiver <- colorBin("YlOrRd", domain = c(min:max))
        minValue <- min(map$variableplot, na.rm = TRUE)
        maxValue <- max(map$variableplot, na.rm = TRUE)

        labels_river <- sprintf("%s: %g", map@data$NAME, map$variableplot) %>% lapply(htmltools::HTML)
        
        # Clear old circle markers
        session$sendCustomMessage("onClearLayerByGroupName", 'g_circle_markers')
        # Show Legends
        session$sendCustomMessage("onShowLegend", '')

        LMap <- leafletProxy(mapId = "map", data = map@data) %>% removeControl(layerId = "Gage")

        for (index in 1:length(map@data$NAME)) {
            LMap <- addCircleMarkers(
                LMap,
                layerId = map@data$NAME[index],
                group = "g_circle_markers",
                lng = keido[index], lat = ido[index],
                data = map$variableplot[index],
                fillColor = getColor(getIndexColor(map$variableplot[index], summary_LOCATION[1], summary_LOCATION[2])), color = "white", dashArray = "2", fillOpacity = 0.7,
                label = sprintf("%s: %g", map@data$NAME[index], map$variableplot[index] ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    group = "g_circle_markers",
                    noHide = F, direction = "bottom", style = list("color" = "yellow", "font-family" = "arial", "box-shadow" = "3px 3px rgba(0,0,0,0.25)", "font-size" = "25px", "border-color" = "rgba(0,0,0,0.5)", "background-color" = "black", "opacity" = 0.65),
                    layerId = map@data$NAME[index]
                )
            )
        }

        LMap %>% fitBounds(~min(keido), ~min(ido), ~max(keido), ~max(ido))
        # Clear old circle markers
        session$sendCustomMessage("onAddedLayerByGroupName", 'g_circle_markers')
    })
}

# For hotline
getSummary <- function() {
    actionMethod <- input$variableselected
    if (actionMethod %in% LOCATION_NAMES) {
        return(summary_LOCATION)
    } else if (actionMethod == '移流分散') {
        return(summary_CT)
    } else if (actionMethod == '到達時間') {
        return(summary_AT)
    } else {
        return(NULL)
    }
}

updateHotLineBuffer <- function () {
    map <- map()
    data <- data()
    actionMethod <- input$variableselected
    targetDate <- input$currentDate
    dataFiltered <- NULL

    if (is.null(data) | is.null(map)) {
        return(NULL)
    }

    if (actionMethod %in% LOCATION_NAMES) {
        onDate <- as.Date(data$YMD)
        dataFiltered <- data[which(onDate == targetDate), ]
        dataFiltered[dataFiltered$ITEM != input$variableselected, 5] <- NA
    } else if (actionMethod == '移流分散' && !is.null(ctData)) {
        dataFiltered <- filter(ctData, ctData$YMD == input$currentDate)
    } else if (actionMethod == '到達時間' && !is.null(atData)) {
        dataFiltered <- atData
    }

    if (!is.null(dataFiltered)) {
        minValue <- min(dataFiltered$VALUE, na.rm = TRUE)
        maxValue <- max(dataFiltered$VALUE, na.rm = TRUE)
        map <- map[which(map$Name %in% dataFiltered$NAME), ]
        dataFiltered <- dataFiltered %>% subset(NAME %>% substring(1,1) %>% is_in(c(LETTERS))) # can start with any letter
        map <- map %>% subset(Name %>% substring(1,1) %>% is_in(c(LETTERS))) # can start with any letter
        mapPoints <- cbind(map$POINT_X, map$POINT_Y, dataFiltered$VALUE, sapply(dataFiltered$NAME, toString))
        summary <- getSummary()
        isTopDown <- actionMethod %in% ACTION_OPTIONS
        results <- list(mapPoints = mapPoints, min = summary[1], max = summary[2], bufferDistance = input$bufferDistance, isTopDown=isTopDown, lat=srclat())
        session$sendCustomMessage("onUpdateHotLineBuffer", results)
    } else {
        session$sendCustomMessage("onAddHotlineNAValue", '')
        session$sendCustomMessage("onAddCircleNAValue", '')
    }
}

updateHotlineSettings <- function () {
    data <- data()
    actionMethod <- input$variableselected
    datafiltered <- NULL

    session$sendCustomMessage("onUpdateHotlineSettings", getHotlineSettings())

    if (actionMethod %in% LOCATION_NAMES) {
        ondate <- as.Date(data$YMD)
        datafiltered <- data[which(ondate == input$currentDate), ]
        datafiltered[datafiltered$ITEM != input$variableselected, 5] <- NA
    } else if (actionMethod == '移流分散' && !is.null(ctData)) {
        datafiltered <- filter(ctData, ctData$YMD == input$currentDate)
    } else if (actionMethod == '到達時間' && !is.null(atData)) {
        datafiltered <- atData
    }

    if (!is.null(datafiltered)) {
        summary <- getSummary()
        if (!is.null(summary)) {
            session$sendCustomMessage('onUpdateColorMarkers', list(data=datafiltered, summary=summary))
        }
    }
}

updateHotline <- function () {
    selectedAction <- input$variableselected

    if (selectedAction %in% c('汚染濃度', '移流分散', '到達時間')) {
        updateHotLineBuffer()
        # updateLabelAT() # removed due to client's request
    } else {
        # Clear hotline if selectedAction is not in c('汚染濃度', '移流分散', '到達時間')
        session$sendCustomMessage('onClearHotline', '')
        # updateLabelAT() # removed due to client's request
    }
}

# Draw Histogram Plot
renderHistogramPlot <- function () {
    data <- data()
    targetDate <- input$currentDate
    actionMethod <- input$variableselected
    dataFiltered <- NULL

    if (!is.null(data) && actionMethod %in% LOCATION_NAMES) {
        onDate <- as.Date(data$YMD)
        dataFiltered <- data[which(onDate == targetDate), ]
        item <- dataFiltered$ITEM
        dataFiltered <- data[which(item == actionMethod), ]
    } else if (actionMethod == '移流分散' && !is.null(ctData)) {
        dataFiltered <- filter(ctData, ctData$YMD == targetDate)
    } else if (actionMethod == '到達時間' && !is.null(atData)) {
        dataFiltered <- atData
    }
    if (!is.null(dataFiltered)) {
        if (actionMethod == "河川流量") label <- "River Flow (m3/s)"
        if (actionMethod == "汚染濃度") label <- "Concentration (mg/m3)"
        if (actionMethod == "移流分散") label <- "Concentration (mg/L)"
        if (actionMethod == "到達時間") label <- "Hours"

        if (!all(is.na(dataFiltered$VALUE))) {
            output$histRiver <- renderPlot({
                hist(as.numeric(dataFiltered$VALUE), main = paste0("Date: ", targetDate), xlab = label, col = '#00DD00', cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.2, border = 'white')
            })
        }
    }
}

updateLegends <- function () {
    actionMethod <- input$variableselected
    LMap <- leafletProxy(mapId = "map")

    LMap %>%
        removeControl(layerId = "legend_discharge") %>%
        removeControl(layerId = "legend_concentration") %>%
        removeControl(layerId = "legend_CT") %>%
        removeControl(layerId = "legend_AT")
    
    if (actionMethod %in% LOCATION_NAMES) {
      map <- map()
        data <- data()
        if (!is.null(map) && !is.null(data)) {
            locationData <- data[data$ITEM == actionMethod]
            valuesOfLocation <- as.numeric(locationData$VALUE)
            minLocation <- min(valuesOfLocation, na.rm = TRUE)
            maxLocation <- max(valuesOfLocation, na.rm = TRUE)
            if (is.na(minLocation) || is.infinite(minLocation)) minLocation = 0
            if (is.na(maxLocation) || is.infinite(maxLocation)) maxLocation = 1
            summary_LOCATION <<- c(minLocation, maxLocation)
            pal_location <<- colorBin(rainbowPallet, domain = c(minLocation, maxLocation), bins=5, na.color = "#808080", pretty=F)

            if (actionMethod == '河川流量') {
                # legend_discharge
                LMap %>% leaflet::addLegend(pal = pal_location, values = valuesOfLocation, opacity = 0.7, title = "河川流量(m3/s)", position = "topleft", layerId = "legend_discharge")
            } 
            else if (actionMethod == '汚染濃度') {
              # legend_concentration
              LMap %>% leaflet::addLegend(pal = pal_location, values = valuesOfLocation, opacity = 0.7, title = "汚染濃度(mg/m3)", position = "topleft", layerId = "legend_concentration")
            }
        }
    } else if (actionMethod == '移流分散' && !is.null(ctData)) {
        # CT data
        ctData$VALUEPLOT <- as.numeric(ctData$VALUE)
        minCT <- min(ctData$VALUEPLOT, na.rm = TRUE)
        maxCT <- max(ctData$VALUEPLOT, na.rm = TRUE)
        if (is.na(minCT)) minCT = 0
        if (is.na(maxCT)) maxCT = 1
        summary_CT <<- c(minCT, maxCT)
        pal_CT <<- colorBin(rainbowPallet, domain = summary_CT, bins=5, na.color = "#808080", pretty=F)

        LMap %>% leaflet::addLegend(pal = pal_CT,  values = ctData$VALUEPLOT,  opacity = 0.7,  title = "移流分散(mg/L)", position = "topleft",  layerId = "legend_CT")
    } else if (actionMethod == '到達時間' && !is.null(atData)) {
        # AT Data
        atData$VALUEPLOT <- as.numeric(atData$VALUE)
        minAT <- min(atData$VALUEPLOT, na.rm = TRUE)
        maxAT <- max(atData$VALUEPLOT, na.rm = TRUE)
        if (is.na(minAT)) minAT = 0
        if (is.na(maxAT)) maxAT = 1
        summary_AT <<- c(minAT, maxAT)
        pal_AT <<- colorBin(rainbowPallet, domain = summary_AT, bins=5, na.color = "#808080", pretty=F)

        LMap %>% leaflet::addLegend(pal = pal_AT, values = atData$VALUEPLOT,  opacity = 0.7,  title = "到達時間(hours)",  position = "topleft",  layerId = "legend_AT")
    }
}

updateLabelAT <- function () {
  # Jan 18 2022: removed this function from updateHotline() since client did not want to have stations labeled with AT
    map <- map()
    map <- data.frame(map)
    actionMethod <- input$variableselected
    lng <- srclng()
    
    # remove first
    LMap <- leafletProxy(mapId = "map")
    LMap %>% clearGroup(group = "labelAT")

    if (!is.null(map) && !is.null(atData) && actionMethod == '到達時間') {
        map %<>% subset(ID %>% substring(1,1) %>% is_in(c(LETTERS))) # can start with any letter
        if (!is.null(lng)) map <- filter(map, map$POINT_X <= lng)
        latList = map$POINT_Y
        lngList = map$POINT_X

        # Add Label
        for (index in 1:length(map$Name)) {
            currentATData <- filter(atData, atData$NAME == map$Name[index])
            if (length(currentATData$NAME) != 0) {
                LMap %>% addLabelOnlyMarkers(lng=lngList[index], lat=latList[index], label=currentATData$LABEL, labelOptions=labelOptions(noHide = T), group="labelAT", layerId=paste0('label_', map$Name[index]))
            }
        }
    }
}

# Set first source location for AT and CT, or find nearest point location
createOrUpdateLatlng <- function () {
    lng <- srclng()
    lat <- srclat()
    map <- map()
    map <- data.frame(map)
    map %<>% subset(ID %>% substring(1,1) %>% is_in(c(LETTERS))) # can start with any letter
    
    locationWithLatlng <- filter(map, map$POINT_X == lng && map$POINT_Y == lat)
    if (length(locationWithLatlng$POINT_X) == 0) {
        locationHasMaxLng <- filter(map, map$POINT_X == max(map$POINT_X))
        session$sendCustomMessage('onClickMap', list(lng=locationHasMaxLng$POINT_X, lat=locationHasMaxLng$POINT_Y))
    }
}

# added by Tak
srcCTAT <- function() {
    actionMethod <- input$variableselected
    if (actionMethod == '移流分散') {
        functionName = 'contaminant transport'
    } else {
        functionName = 'arrival time'
    }
    session$sendCustomMessage('onToggleCalculateCTAT', TRUE)
    showNotification(paste0('計算中 ', functionName, ' ...'), id="msgCaculateCTAT")
    # calculate contaminant transport and arrival time from source to all stations 
    # type="river": Distance is measured with river segments ("Distance by boat") 
    # by.hour=T, calculate by hour, takes longer but more accurate
    
    # output:
    # - $CT is a long data frame {ID, ID_No, day, CT}
    # - $AT is {Name, ID_No, ID, POINT_X, POINT_Y, AT (days, num), AT_label (daysHours, char)}
    
    map <- map()
    data <- data() # need data to get first date
    
    # if (input$variableselected %in% ACTION_OPTIONS) map %<>% subset(ID %>% substring(1,1) %>% equals("Y"))
    if (input$variableselected %in% ACTION_OPTIONS) map %<>% subset(ID %>% substring(1,1) %>% is_in(c(LETTERS)))
    srclat <- srclat()
    srclng <- srclng()
    CT_thrs <- CT_thrs()
    CT_tmax <- CT_tmax()
    CT_mass <- CT_mass()
    CT_area <- CT_area()
    CT_disp <- CT_disp()
    CT_velo <- CT_velo()
    CT_gamm <- CT_gamm()
    
    sec2day <- SECONDS_ONE_DAY
    CT_maxt <- CT_tmax * sec2day
    arrivalTimeUnit <- arrivalTimeUnit()
    
    calcDist <- function() {
        # calculate distance (internal function)
        
        map <- map()
        
        if (input$variableselected %in% ACTION_OPTIONS) map %<>% subset(ID %>% substring(1,1) %>% is_in(c(LETTERS))) # can start with any letter
        srclat <- srclat()
        srclng <- srclng()
        CT_thrs <- CT_thrs()
        CT_tmax <- CT_tmax()
        CT_mass <- CT_mass()
        CT_area <- CT_area()
        CT_disp <- CT_disp()
        CT_velo <- CT_velo()
        CT_gamm <- CT_gamm()
        
        sec2day <- SECONDS_ONE_DAY
        CT_maxt <- CT_tmax * sec2day
        arrivalTimeUnit <- arrivalTimeUnit()
        
        # if (type=="river") {
        L <- map %>% nrow
        D <- data.frame(coordinates(map),PID=1,POS=1:L)
        
        # replace first point with mouse click, and remove other points upstream
        if (!is.null(srclat) & input$variableselected %in% ACTION_OPTIONS) {
            m <- which.min(abs(D$coords.x1 - srclng))
            D %<>% slice(m:L)
            L <- nrow(D)
        } 
        colnames(D)[1:2] <- c("X","Y")
        D %<>% as.PolySet(projection = "LL")
        out <- numeric(L)
        for (i in 1:(L-1)) { # i=1
            out[i] <- calcLength(D) %>% select(length) %>% as.numeric
            D %<>% slice(1:(nrow(D)-1))
        }
        return(out %>% rev %>% multiply_by(1000))
    }

    calcCT <- function(tim,i) { # i=1
        # calculate CT (internal function)
        
        map <- map()
        
        if (input$variableselected %in% ACTION_OPTIONS) map %<>% subset(ID %>% substring(1,1) %>% is_in(c(LETTERS))) # can start with any letter
        srclat <- srclat()
        srclng <- srclng()
        CT_thrs <- CT_thrs()
        CT_tmax <- CT_tmax()
        CT_mass <- CT_mass()
        CT_area <- CT_area()
        CT_disp <- CT_disp()
        CT_velo <- CT_velo()
        CT_gamm <- CT_gamm()

        sec2day <- SECONDS_ONE_DAY
        CT_maxt <- CT_tmax * sec2day
        arrivalTimeUnit <- arrivalTimeUnit()
        
        CT_dist <- calcDist()
        CT <- array(0:as.integer(CT_maxt / sec2day), dim = c(1))
        k <- 1
        
        for(l in tim) { # l=tim[2]
            mbya <- CT_mass / CT_area
            val1 <- mbya / sqrt(4 * pi * CT_disp * l)
            val2 <- -1.0 * (CT_dist[i] - CT_velo * l) * (CT_dist[i] - CT_velo * l) / (4 * CT_disp * l)
            hfnc <- hermite(3, kind = "h")
            z    <- (CT_dist[i] - CT_velo * l) / sqrt(2 * CT_disp * l)
            vtmp <- data.frame(1, as.function(hfnc)(z)) %>% suppressMessages()
            val3 <- 1 - (CT_gamm * vtmp)
            CT[k] <- val1 * exp(val2) * val3[1, 1] * 1e6 # unit microns (mg/L)
            k <- k + 1  
        }
        return(CT)
    }

    # replace first point with mouse click, and remove other points upstream
    if (!is.null(srclat) & input$variableselected %in% ACTION_OPTIONS) {
        L <- map %>% nrow
        m <- which.min(abs(map$coords.x1-srclng))
        map <- map[m:L,]
    }
    
    # Get CT and AT for all stations
    tm0 <- proc.time()[3]
    
    tim <- seq(0, CT_maxt, sec2day)
    m1 <- m2 <- NULL
    # L2 <- map %>% nrow  
    L2 <- calcDist() %>% length
    tm <- proc.time()[3]

    m0 <- map %>% 
        as.data.frame %>% 
        select(-c(coords.x1, coords.x2,ID_No,ID)) %>% 
        slice(rep(row_number(), each = CT_tmax+1))
    m0$No <- CT_tmax %>% seq(0) %>% rev %>% rep(L2) %>% add(1)
    m0$YMD <- as.Date(paste0(as.Date(data$YMD[1]) %>% substring(1,8),CT_tmax %>% seq(0) %>% rev %>% rep(L2) %>% add(1)))
    m0$ITEM <- "移流分散"
    names(m0)[1] <- "NAME"

    # calculate CT and AT
    for (i in 1:L2) { # i=1
        CT <- tim %>% calcCT(i)
        m1 %<>% c(CT)
        AT <- tim[(CT > CT_thrs) %>% which %>% min] %>% divide_by(sec2day)
        AT_label <- AT %>% multiply_by(sec2day) %>% seconds_to_period %>% str_split(" 0") %>% unlist %>% nth(1)
        if (arrivalTimeUnit == "hours") { # calculate AT by the hour
            ind0 <- (CT > CT_thrs) %>% which %>% min # first guess
            if (!is.finite(ind0)) ind0 <- 2
            # tmp <- tim[ind0 - 1]
            # if(!is.finite(tmp)) tim0 <- c(0,seq(0,sec2day,sec2day/24))
            # if(is.finite(tmp)) 
            tim0 <- c(0,seq(tim[ind0-1], tim[ind0], sec2day/24)) # range of guess
            CT0 <- tim0 %>% calcCT(i) # CT of range
            AT <- tim0[(CT0 > CT_thrs) %>% which %>% min] %>% divide_by(sec2day)
            AT_label <- AT %>% multiply_by(sec2day) %>% seconds_to_period %>% str_split(" 0") %>% unlist %>% nth(1)
        }
        m2 %<>% rbind(data.frame(map[i,],VALUE=AT,LABEL=AT_label) )
    }

    m1 <- data.frame(m0,VALUE=m1) %>% select(No,NAME,ITEM,YMD,VALUE)
    names(m2)[1] <- "NAME"
    m2$ITEM <- "到達時間"
    m2$YMD <- as.Date(data$YMD[1]) # first date depends on data
    m2$No <- 1
    m2 %<>% select(No,NAME,ITEM,YMD,VALUE,LABEL)
    
    # output
    out <- NULL
    out$CT <- m1
    out$AT <- m2
    out$time <- proc.time()[3] - tm0

    session$sendCustomMessage('onToggleCalculateCTAT', FALSE)
    removeNotification('msgCaculateCTAT')
    showNotification(paste0('計算完了'), type="message")
    return(out)
}

