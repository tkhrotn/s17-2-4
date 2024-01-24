######################################################################
#
# Contamination Transport & Arrival Time
#
######################################################################

#  Import libraries ---------------------
pacman::p_load(magrittr,dplyr,PBSmapping,stringr,lubridate,readxl,xts)

# GLOBAL VARIABLES ---------------------

# geojson data
geojsonList <- list()
summary_LOCATION <- NULL
summary_AT <- NULL
summary_CT <- NULL
ctData <- NULL
atData <- NULL

# HELPER FUNCTIONS ---------------------

eval(parse("components/Map/helpers/CTAT.R", encoding="UTF-8"))


# INITIALIZE UI ---------------------

# initialize srcROI value
shinyjs::disable("calcATCT")
updateNumericInput(session, 'srcROI', value=radiusInfluence)
updateSelectInput(session, 'mapselected', choices=names(MAP_NAME_OPTIONS), selected=names(MAP_NAME_OPTIONS)[[1]])
updateSelectInput(session, 'variableselected', choices=ALL_ACTION_OPTIONS, selected=ALL_ACTION_OPTIONS[1])
session$sendCustomMessage("onUpdateHotlineSettings", getHotlineSettings())


# REACTIVE ---------------------

data <- reactive({
    # Required input
    req(input$csvfile)
    return(readDataLocation(input$csvfile$datapath))
})

map <- reactive({
    # Required input
    req(input$shpfile)
    map <- readShapefiles(input$shpfile, TRUE, session)
    return(map)
})

map_new_data <- reactive({
    # Required input
    req(input$shpfile2)
    map2 <- readShapefiles(input$shpfile2, FALSE, session)
    return(map2)
})

mapNo <- reactive(input$radio)

# Find the nearest point on Map
srclat <- reactive({
    actionMethod <- input$variableselected
    latlng <- input$map_click
    map <- map()

    if (actionMethod %in% ACTION_OPTIONS && !is.null(map()) && input$tabs == 'ctat') {
        map %<>% subset(ID %>% substring(1,1) %>% is_in(c(LETTERS)))   
        result <- list(mapPoints=cbind(map$POINT_X, map$POINT_Y), fromLatlng=latlng)
        session$sendCustomMessage('onFindNearestPoint', result)
        return(latlng$lat)
    } else {
        return(latlng$lat)
    }
})
srclng <- reactive(input$map_click$lng)
srcROI <- reactive(input$srcROI)

CT_thrs <- reactive({CT_thrs <- as.numeric(input$CT_thrs); return(CT_thrs); })
CT_tmax <- reactive({CT_tmax <- as.numeric(input$CT_tmax); return(CT_tmax); })
CT_mass <- reactive({CT_mass <- as.numeric(input$CT_mass); return(CT_mass); })
CT_area <- reactive({CT_area <- as.numeric(input$CT_area); return(CT_area); })
CT_disp <- reactive({CT_disp <- as.numeric(input$CT_disp); return(CT_disp); })
CT_velo <- reactive({CT_velo <- as.numeric(input$CT_velo); return(CT_velo); })
CT_gamm <- reactive({CT_gamm <- as.numeric(input$CT_gamm); return(CT_gamm); })


arrivalTimeUnit <- reactive({
    if (input$variableselected %in% c("移流分散")) {
        return('days')
    } else {
        return('hours')
    }
})

# OUTPUT ---------------------

# Map
output$map <- renderLeaflet({
    # Required shapefile and data location to show Map
    map <- map()
    data <- data()
    
    if (is.null(data) | is.null(map)) {
        return(NULL)
    }

    # Initial Map
    LMap <- buildMap(map) #%>% add
    
    # Adding geojson
    for (index in seq_along(geojsonList)) {
        LMap <- addPolygons(LMap, data = geojsonList[[index]])
    }

    return(LMap)
})


# 発災地情報
output$latbox <- renderInfoBox({
    srclat <- srclat()
    infoBox("緯度", srclat, icon = icon("map", lib = "font-awesome"), color = "light-blue", fill = TRUE, width = 3)
})

output$lngbox <- renderInfoBox({
    srclng <- srclng()
    infoBox("緯度", srclng, icon = icon("map", lib = "font-awesome"), color = "light-blue", fill = TRUE, width = 3)
})


# OBSERVE ---------------------

# 日付を選択する
observe({
    data <- data()

    if (!is.null(data)) {
        dates <- as.Date(data$YMD)
        min <- min(dates)
        max <- max(dates)
        updateDateInput(session = session, inputId = 'currentDate', value = min, min = min, max = max)
    }
})

# Change Map tile
observe({
    selectedMapName <- input$mapselected
    if (!(is.element(selectedMapName, names(MAP_NAME_OPTIONS)))) {
        return(NULL)
    }
    isolate({
        selectedMap <- MAP_NAME_OPTIONS[[selectedMapName]]
        basemap <<- MAP_TILES_URLS[[selectedMap]]
        til <<- MAP_TILES_DESCRIPTION[[selectedMap]]

        leafletProxy(mapId="map") %>%
            clearTiles() %>%
            addTiles(basemap, attribution=til)
    })
})

# Histogram Plot
observe({
    data <- data()
    input$currentDate
    input$variableselected

    renderHistogramPlot()
})

# Change action method
observe({
    actionMethod <- input$variableselected
    data <- data()
    map <- map()
    if (is.null(data) | is.null(map)) {
        return(NULL)
    }

    isolate({
        updateLegends()
        updateHotline()
        if (is.element(actionMethod, LOCATION_NAMES)) {
            updateCircleMarkers()
        }
        if (actionMethod %in% ACTION_OPTIONS) {
            shinyjs::enable("calcATCT")
            createOrUpdateLatlng()
        } else {
            shinyjs::disable("calcATCT")
        }
        if (actionMethod == '到達時間') {
            shinyjs::disable("currentDate")
        } else {
            shinyjs::enable("currentDate")
        }
    })
})

# Hotline data changed
observe({
    map <- map() 
    data <- data()
    input$currentDate
    input$bufferDistance
    actionMethod <- input$variableselected

    if (is.null(map()) | is.null(data())) {
        return(NULL)
    }

    isolate({
        # Update hotline settings: Color, outlinewidth, outlineColor, ...
        updateHotlineSettings()
        updateLegends()
        updateHotline()
    })
})

# Reactive display Radius Of Influence (ROI)
observe({
    if (is.null(srclat()) | is.null(srclng()) | is.null(srcROI()) | is.null(map())) {
        return(NULL)
    }
    isolate({
        map <- map()
        srclat <- srclat()
        srclng <- srclng()
        srcROI <- srcROI()
        currentTab <- input$tabs
        if (currentTab == 'ctat') {
            leafletProxy(mapId = "map") %>%
            removeControl(layerId = "srclatlng") %>%
            removeControl(layerId = "srcROI") %>%
            addCircles(lng = srclng, lat = srclat, radius = 10.0, color = "red", dashArray = "2", fillOpacity = 1.0, layerId = "srclatlng") %>%
            addCircles(lng = srclng, lat = srclat, fillColor = F, radius = srcROI, color = "red", dashArray = "2", fillOpacity = 0.3, layerId = "srcROI")
            session$sendCustomMessage("onBringToBackClickablePoint", '')  
        }
    })
})

# Change param by upload CSV
observe({
    paramCSV <- input$csvfile2
    isolate({
        if (is.null(paramCSV))
          return();
        dataParam <- readParam(paramCSV$datapath)
        # Update inputs
        updateNumericInput(session, 'srcROI', value=as.numeric(dataParam[dataParam$PARAM_NAME == "srcROI",]["VALUE"]))
        updateNumericInput(session, 'CT_thrs', value=as.numeric(dataParam[dataParam$PARAM_NAME == "CT_thrs",]["VALUE"]))
        updateNumericInput(session, 'CT_tmax', value=as.numeric(dataParam[dataParam$PARAM_NAME == "CT_tmax",]["VALUE"]))
        updateNumericInput(session, 'CT_mass', value=as.numeric(dataParam[dataParam$PARAM_NAME == "CT_mass",]["VALUE"]))
        updateNumericInput(session, 'CT_area', value=as.numeric(dataParam[dataParam$PARAM_NAME == "CT_area",]["VALUE"]))
        updateNumericInput(session, 'CT_disp', value=as.numeric(dataParam[dataParam$PARAM_NAME == "CT_disp",]["VALUE"]))
        updateNumericInput(session, 'CT_velo', value=as.numeric(dataParam[dataParam$PARAM_NAME == "CT_velo",]["VALUE"]))
        updateNumericInput(session, 'CT_gamm', value=as.numeric(dataParam[dataParam$PARAM_NAME == "CT_gamm",]["VALUE"]))
    })
})


# OBSERVE EVENT ---------------------
# To reset rivers/lakes when toggled off
observe({
  if (!any(input$map_groups %in% c("PRTR事業所➔河川","河川➔PRTR事業所"))) {
    leafletProxy(mapId = "map") %>% clearGroup(group = "rivers.sewage") %>% clearGroup(group = "rivers.public") %>% clearGroup(group = "upper.sewage") %>% clearGroup(group = "upper.public") %>% clearGroup(group = "lakes.public") %>% clearGroup(group = "lakes.sewage") %>% clearGroup(group = "prtr.sewage") %>% clearGroup(group = "prtr.public") %>% clearGroup(group = "prtr.both")} 
})


# Click PRTR station to show linked rivers/lakes
observe({
  # Click on marker
  marker <- input$map_marker_click
  isolate({
    if (!is.null(marker$group)) {
      if (marker$group %in% c("PRTR事業所➔河川")) { # if (marker$group == "PRTR事業所") {
        LMap <- leafletProxy(mapId = "map")
        
        # # remove previous rivers/lakes by group name
        LMap %>% clearGroup(group = "rivers.sewage") %>% clearGroup(group = "rivers.public") %>% clearGroup(group = "rivers.both") %>% clearGroup(group = "upper.sewage") %>% clearGroup(group = "upper.public") %>% clearGroup(group = "upper.both") %>% clearGroup(group = "lakes.public") %>% clearGroup(group = "lakes.sewage") %>% clearGroup(group = "lakes.both") 
        
        # get the clicked station 
        rivers_old <- prtr_river_old
        rivers_new <- prtr_river_new
        rivers_all <- prtr_river_all
        lakes <- prtr_lake
        lr <- lake_river_id
        # if marker starts with kanji, reassign marker as just the kanji
        if (grepl("#", rivers_new$W05_004[1])) rivers_new$W05_004 <- gsub("-.*","",rivers_new$W05_004)
        prtr_1 <- prtr_station1[which(prtr_station1$ID == marker$id | as.character(prtr_station1$upper) == marker$id),] 
        prtr_2 <- prtr_station2[which(prtr_station2$ID == marker$id),]
        
        
        # - river ID 860610838 is uppermost stream for 近江八幡市 area -> PRTR is 2015, River database is 2018/10. Maybe rivers were removed?
        # Other PRTR stations with missing upstream river are:
        # P1625004-00033-00        # E1625004-00063-00        # E1625004-00069-00        # E1625004-00027-02        # E1625004-00033-02        # E1625004-00043-00        # P1625004-00010-00        # E1625004-00049-00
        # P1625004-00011-00        # P1625004-00027-00        # P1625004-00032-00        # P1625004-00035-00        # E1625004-00054-00        # P1625004-00008-00        # P1625004-00028-00
        # Three categories of kasen type　# 1=public, 2=sewage, 3=both
        prtr_public <-     prtr_data_old[which(prtr_data_old$ID == prtr_1$ID & prtr_data_old$public==1),]
        prtr_sewage <-     prtr_data_old[which(prtr_data_old$ID == prtr_1$ID & prtr_data_old$public==2),]
        prtr_both <-       prtr_data_old[which(prtr_data_old$ID == prtr_1$ID & prtr_data_old$public==3),]
        prtr_public_new <- prtr_data_new[which(prtr_data_new$linkID == prtr_2$linkID),]
        rivers.public <- rivers_old[which(rivers_old$LinkID %in% prtr_public$linkID),]
        rivers.sewage <- rivers_old[which(rivers_old$LinkID %in% prtr_sewage$linkID),]
        rivers.both   <- rivers_old[which(rivers_old$LinkID %in% prtr_both$linkID),]
        rivers.public_new <- rivers_new[which(rivers_new$W05_004 %in% prtr_public_new$linkID),]

        if (nrow(rivers.both)==0 & nrow(rivers.public)!=0 & nrow(rivers.sewage)==0) {
          # public, upper public
          LMap %>%
            addPolylines(data=rivers.public,
                         group="rivers.public",
                         color="blue",
                         weight="8",
                         opacity = 0.5) %>%
            addPolylines(data=rivers_old[which(rivers_old$LinkID %in% prtr_public$upper[1]),], # upper.public,
                         group="upper.public",
                         color="blue",
                         weight="6",
                         opacity = 0.7) %>%
            addPolygons(data=lakes[which(lakes$LinkID %in% (lr[which(lr$riverID %in% (rivers.public$LinkID %>% as.numeric)),"lakeID"] %>% unlist %>% as.character)),],
                        group="lakes.public",
                        color="blue",
                        weight=1,
                        fillOpacity = 0.8) %>% 
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        } %>% suppressWarnings()
        
        if (nrow(rivers.public_new)!=0) {
          # New PRTR/River data
          LMap %>%
            addPolylines(data=rivers.public_new,
                         group="rivers.public",
                         color="blue",
                         weight="6",
                         opacity = 0.5) %>%
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        } %>% suppressWarnings()

        if (nrow(rivers.both)==0 & nrow(rivers.public)==0 & nrow(rivers.sewage)!=0) {
          # sewage, upper sewage
          LMap %>%
            addPolylines(data=rivers.sewage,
                         group="rivers.sewage",
                         color="yellow",
                         weight="8",
                         opacity = 0.5) %>%
            addPolylines(data=rivers_old[which(rivers_old$LinkID %in% prtr_sewage$upper[1]),], #upper.sewage,
                         group="upper.sewage",
                         color="yellow",
                         weight="6",
                         opacity = 0.7) %>%
            addPolygons(data=lakes[which(lakes$LinkID %in% (lr[which(lr$riverID %in% (rivers.sewage$LinkID %>% as.numeric)),"lakeID"] %>% unlist %>% as.character)),],
                        group="lakes.sewage",
                         color="yellow",
                         weight=1,
                         fillOpacity = 0.8) %>% 
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        } %>% suppressWarnings()
        
        if (nrow(rivers.both)!=0 & nrow(rivers.public)==0 & nrow(rivers.sewage)==0) {
          # both
          LMap %>%
            addPolylines(data=rivers.both,
                         group="rivers.both",
                         color="green",
                         weight="8",
                         opacity = 0.5) %>%
            addPolylines(data=rivers_old[which(rivers_old$LinkID %in% prtr_both$upper[1]),], #upper.both,
                         group="upper.both",
                         color="green",
                         weight="6",
                         opacity = 0.5) %>%
            addPolygons(data=lakes[which(lakes$LinkID %in% (lr[which(lr$riverID %in% (rivers.both$LinkID %>% as.numeric)),"lakeID"] %>% unlist %>% as.character)),],
                        group="lakes.both",
                         color="green",
                         weight=1,
                         fillOpacity = 0.8) %>% 
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        } %>% suppressWarnings()
        
        if (nrow(rivers.both)==0 & nrow(rivers.public)!=0 & nrow(rivers.sewage)!=0) {
          # both public and sewage, upper public and sewage
          LMap %>%
          addPolylines(data=rivers.public,
                       group="rivers.public",
                       color="green",
                       weight="8",
                       opacity = 0.5) %>%
          addPolylines(data=rivers_old[which(rivers_old$LinkID %in% prtr_public$upper[1]),], #upper.public,
                       group="upper.public",
                       color="green",
                       weight="6",
                       opacity = 0.5) %>%
            addPolygons(data=lakes[which(lakes$LinkID %in% (lr[which(lr$riverID %in% (rivers.public$LinkID %>% as.numeric)),"lakeID"] %>% unlist %>% as.character)),],
                        group="lakes.public",
                        color="green",
                        weight=1,
                        fillOpacity = 0.8) %>% 
            addPolylines(data=rivers.sewage,
                       group="rivers.sewage",
                       color="green",
                       weight="8",
                       opacity = 0.7) %>%
            addPolylines(data=rivers_old[which(rivers_old$LinkID %in% prtr_sewage$upper[1]),], #upper.sewage,
                       group="upper.sewage",
                       color="green",
                       weight="6",
                       opacity = 0.7) %>%
            addPolygons(data=lakes[which(lakes$LinkID %in% (lr[which(lr$riverID %in% (rivers.sewage$LinkID %>% as.numeric)),"lakeID"] %>% unlist %>% as.character)),],
                        group="lakes.sewage",
                        color="green",
                        weight=1,
                        fillOpacity = 0.8) %>% 
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        } %>% suppressWarnings()

        
      }
    }
  })
})

# Click river to show linked PRTR stations
observe({
  # Click on marker
  marker <- input$map_shape_click
  isolate({
    if (!is.null(marker$group)) {
      if (marker$group %in% c("河川➔PRTR事業所")) { 
        LMap <- leafletProxy(mapId = "map")
        # # remove previous rivers/lakes by group name
        LMap %>% clearGroup(group = "prtr.sewage") %>% clearGroup(group = "prtr.public") %>% clearGroup(group = "prtr.both")  
        
        # get the clicked river/lake
        rivers_old <- prtr_river_old
        rivers_new <- prtr_river_new
        rivers_all <- prtr_river_all
        lakes <- prtr_lake
        lr <- lake_river_id
        # if marker starts with kanji, reassign marker as just the kanji
        if (grepl("#", rivers_new$W05_004[1])) {
          rivers_new$W05_004 <- gsub("-.*","",rivers_new$W05_004)
          marker$id <- gsub("-.*","",marker$id)
        }
        prtr_1 <- prtr_station1[which(prtr_station1$ID == marker$id | as.character(prtr_station1$upper) == marker$id),] 
        prtr_2 <- prtr_station2[which(prtr_station2$linkID == marker$id),]
      
        # if lake polygon is clicked
        if (as.character(marker$id) %in% as.character(lr$lakeID)) {
          prtr_1 <- prtr_station1[which(prtr_station1$upper %in% lr$riverID[lr$lakeID %in% lr$lakeID %>% which]),]
          prtr_2 <- prtr_station2[which(prtr_station2$upper %in% lr$riverID[lr$lakeID %in% lr$lakeID %>% which]),]
        }
        
        # Three categories of prtr based on kasen type　# 1=public, 2=sewage, 3=both
        prtr_public <-     prtr_data_old[which(prtr_data_old$ID %in% prtr_1$ID & prtr_data_old$public==1),]
        prtr_sewage <-     prtr_data_old[which(prtr_data_old$ID %in% prtr_1$ID & prtr_data_old$public==2),]
        prtr_both <-       prtr_data_old[which(prtr_data_old$ID %in% prtr_1$ID & prtr_data_old$public==3),]
        prtr_public_new <- prtr_data_new[which(prtr_data_new$linkID %in% prtr_2$linkID),]
        
        if (nrow(prtr_public)!=0 & nrow(prtr_sewage)==0 & nrow(prtr_both)==0) {
          LMap %>%
          addCircleMarkers(data=prtr_public,
                           lng = prtr_public$lng, 
                           lat = prtr_public$lat,
                           dashArray = "2",
                           radius=7,
                           fillColor="blue",
                           color="purple",
                           fillOpacity=1,
                           opacity=1,
                           #weight=0.5,
                           label = paste(prtr_public$company.name1, prtr_public$address1,prtr_public$address2,prtr_public$address3) %>% lapply(htmltools::HTML),
                           group="prtr.public",
                           labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>%
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        }
        if (nrow(prtr_public_new)!=0) { 
          LMap %>%
            addCircleMarkers(data=prtr_public_new,
                             lng = prtr_public_new$lng,
                             lat = prtr_public_new$lat,
                             dashArray = "2",
                             radius=7,
                             fillColor="grey",
                             color="purple",
                             fillOpacity=1,
                             opacity=1,
                             #weight=0.5,
                             label = paste(prtr_public_new$company.name1, prtr_public_new$address1,prtr_public_new$address2,prtr_public_new$address3) %>% lapply(htmltools::HTML),
                             group="prtr.public",
                             labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>%
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        }
        
        
        if (nrow(prtr_public)==0 & nrow(prtr_sewage)!=0 & nrow(prtr_both)==0) {
          LMap %>%
            addCircleMarkers(data=prtr_sewage,
                             lng = prtr_sewage$lng, 
                             lat = prtr_sewage$lat,
                             dashArray = "2",
                             radius=7,
                             fillColor="yellow",
                             color="purple",
                             fillOpacity=1,
                             opacity=1,
                             #weight=0.5,
                             label = paste(prtr_sewage$company.name1, prtr_sewage$address1,prtr_sewage$address2,prtr_sewage$address3) %>% lapply(htmltools::HTML),
                             group="prtr.sewage",
                             labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>%
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        }

        if (nrow(prtr_public)==0 & nrow(prtr_sewage)==0 & nrow(prtr_both)!=0) {
          LMap %>%
            addCircleMarkers(data=prtr_both,
                             lng = prtr_both$lng, 
                             lat = prtr_both$lat,
                             dashArray = "2",
                             radius=7,
                             fillColor="green",
                             color="purple",
                             fillOpacity=1,
                             opacity=1,
                             #weight=0.5,
                             label = paste(prtr_both$company.name1, prtr_both$address1,prtr_both$address2,prtr_both$address3) %>% lapply(htmltools::HTML),
                             group="prtr.both",
                             labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>%
            leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        }
        
        if (nrow(prtr_public)!=0 & nrow(prtr_sewage)!=0 & nrow(prtr_both)==0) {
          LMap %>%
            addCircleMarkers(data=prtr_public,
                             lng = prtr_public$lng, 
                             lat = prtr_public$lat,
                             dashArray = "2",
                             radius=7,
                             fillColor="green",
                             color="purple",
                             fillOpacity=1,
                             opacity=1,
                             #weight=0.5,
                             label = paste(prtr_public$company.name1, prtr_public$address1,prtr_public$address2,prtr_public$address3) %>% lapply(htmltools::HTML),
                             group="prtr.both",
                             labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>%
            addCircleMarkers(data=prtr_sewage,
                             lng = prtr_sewage$lng, 
                             lat = prtr_sewage$lat,
                             dashArray = "2",
                             radius=7,
                             fillColor="green",
                             color="purple",
                             fillOpacity=1,
                             opacity=1,
                             #weight=0.5,
                             label = paste(prtr_sewage$company.name1, prtr_sewage$address1,prtr_sewage$address2,prtr_sewage$address3) %>% lapply(htmltools::HTML),
                             group="prtr.both",
                             labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>%
          leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        }
        
        if (nrow(prtr_public)!=0 & nrow(prtr_sewage)!=0 & nrow(prtr_both)!=0) {
          LMap %>%
            addCircleMarkers(data=prtr_public,
                             lng = prtr_public$lng, 
                             lat = prtr_public$lat,
                             dashArray = "2",
                             radius=7,
                             fillColor="green",
                             color="purple",
                             fillOpacity=1,
                             opacity=1,
                             #weight=0.5,
                             label = paste(prtr_public$company.name1, prtr_public$address1,prtr_public$address2,prtr_public$address3) %>% lapply(htmltools::HTML),
                             group="prtr.both",
                             labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>%
            addCircleMarkers(data=prtr_sewage,
                             lng = prtr_sewage$lng, 
                             lat = prtr_sewage$lat,
                             dashArray = "2",
                             radius=7,
                             fillColor="green",
                             color="purple",
                             fillOpacity=1,
                             opacity=1,
                             #weight=0.5,
                             label = paste(prtr_sewage$company.name1, prtr_sewage$address1,prtr_sewage$address2,prtr_sewage$address3) %>% lapply(htmltools::HTML),
                             group="prtr.both",
                             labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>% 
            addCircleMarkers(data=prtr_both,
                             lng = prtr_both$lng, 
                             lat = prtr_both$lat,
                             dashArray = "2",
                             radius=7,
                             fillColor="green",
                             color="purple",
                             fillOpacity=1,
                             opacity=1,
                             #weight=0.5,
                             label = paste(prtr_both$company.name1, prtr_both$address1,prtr_both$address2,prtr_both$address3) %>% lapply(htmltools::HTML),
                             group="prtr.both",
                             labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "PRTR_r")) %>%
          leaflet::addLegend(colors = c("blue", "yellow","green","grey"), labels = c("公共用水域","下水道","両方","不明"), bins=2,opacity = 0.7, title = "河道タイプ", position = "topleft", layerId = "legend_PRTR")
        } 
      
        
      }
    }
  })
})

# If certain box is ticked, jump to the appropriate location:
# If Air or River simulation is chosen, go to that location
observe({
  if (any(input$map_groups %in% c("大気シミュレーション"))) {
    leafletProxy(mapId = "map") %>% setView(lng=136.9383015, lat=35.1387526, zoom=17) }
})

observe({
  if (any(input$map_groups %in% c("河川シミュレーション"))) {
    leafletProxy(mapId = "map") %>% setView(lng=135.8193, lat=35.4842, zoom=13) }
})

observe({
  if (any(input$map_groups %in% c("PRTR事業所➔河川","河川➔PRTR事業所"))){
    leafletProxy(mapId = "map") %>% setView(lng=135.79981, lat=34.88436, zoom=10) }
})

observe({
  if (!(any(input$map_groups %in% c("大気シミュレーション","河川シミュレーション","PRTR事業所➔河川","河川➔PRTR事業所")))){
    leafletProxy(mapId = "map") %>% setView(lng=135.50352, lat=34.73676, zoom=12) }
})


# Click to Time Player
observeEvent(input$runAnimationRiver, {
    data <- data()
    if (is.null(data)) {
        return(NULL)
    }
    session$sendCustomMessage("runAnimationRiver", getResults(data$YMD, input$currentDate, 5))
})

# Calculate Contamination Transport
observeEvent(input$calcATCT, {
    data <- data()
    map <- map()    
    actionMethod <- input$variableselected

    if (!is.null(data) && !is.null(map) && (actionMethod %in% ACTION_OPTIONS)) {
        tryCatch({
            map <- data.frame(map)
            # map %<>% subset(ID %>% substring(1,1) %>% equals("Y"))
            map %<>% slice(which(gsub("[[:digit:]]", "", map$ID) %in% c(LETTERS))) # more letters possible
            map <- filter(map, map$POINT_X <= srclng())
            if (length(map$Name) <= 1) {
                session$sendCustomMessage("onAddHotlineNAValue", '')
                session$sendCustomMessage("onAddCircleNAValue", '')
            } else {
              # browser()
                ctAtData <- srcCTAT()
                if (actionMethod == '移流分散') {
                    ctData <<- ctAtData$CT
                } else if (actionMethod == '到達時間') {
                  ctAtData$AT$VALUE %<>% multiply_by(24)   
                  atData <<- ctAtData$AT
                }
                updateLegends()
                renderHistogramPlot()
                updateHotlineSettings()
                updateHotline()
            }
        }, error = function (err) {
            session$sendCustomMessage('onToggleCalculateCTAT', FALSE)
            print(err)
        })
    }
})


# Save ATCT output as CSV file 
output$saveATCT = downloadHandler(
  filename = "TMDU_RIVER_ATCT.csv",
  content = function(file) {
    ctAtData <- srcCTAT()
    AT <- ctAtData$AT
    CT <- ctAtData$CT
    CT %>% rbind(AT %>% select(-LABEL)) %>% select(-No) %>% fwrite(file=file, col.names = T,bom=T)
  }
) 

# Save map as SHP file
# output$saveSHP = downloadHandler(
#   filename = "TMDU_RIVER.shp",
####   filename = "TMDU_RIVER.geojson",
#   content = function(file) {
#     blah <- the map...
#     blah %>% writeOGR(file=file, dsn=".", driver = "ESRI Shapefile")
#     blah %>% writeOGR(file=file, dsn=".", driver = "GeoJSON")
#     blah %>% geojson_write(geometry = "LINESTRING", file = file)
#     blah %>% st_as_sf(coords = c("x", "y"), crs = 28992, agr = "constant") %>% st_write(file=file) # for shape or geojson

#   }
# ) 

# NOT USED NOW ---------------------

# Supplemental example of Mapdeck 3D visualization(Not used)
output$'3Dmap' <- renderMapdeck({
    mapdeck(
        style = mapdeck_style('dark'), pitch = 45) %>%
        add_line(data = df, layer_id = "line_layer", origin = c("upLong", "upLat"), destination = c("dnLong", "dnLat"), stroke_colour = "upVal", stroke_width = "upVal")
})

output$histSupply <- renderPlot({
    water_supply0 <- read.csv("data/static-data/water_supplu_H24(H22).csv", fileEncoding = "SJIS")
    hist(as.numeric(water_supply0$'日最大給水量'/1000), main = "日最大給水量", xlab = "日最大給水量 (X 1000 m3)", breaks = seq(0,2000,250), col = '#00DD00', cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.2, border = 'white')
})

# ????

pointCount <- 0
observe({
    if (is.null(mapNo()) | is.null(map())) {
        return(NULL)
    }

    isolate({
        map <- map()
        mapNo <- mapNo()

        if (mapNo == 3) {
            targetData <- wells
            label <- labels_wells
            XXX <- wells$X
            YYY <- wells$Y
            index <- 1:nrow(wells)
            ccolors <- "cyan"
            chrTitle   <- "水道用井戸"
        } else if (mapNo == 1) {
            targetData <- water_supply
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