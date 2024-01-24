######################################################################
#
# Groundwater 
#
######################################################################

# IMPORT LIBRARIES ---------------------
pacman::p_load(magrittr,dplyr,ContourFunctions)

# GLOBAL VARIABLES ---------------------
outputContour <- NULL

# HELPER FUNCTIONS ---------------------
eval(parse("components/Map/helpers/Groundwater.R", encoding="UTF-8"))

# INITIALIZE UI ---------------------
# shinyjs::disable("calculate")
shinyjs::disable("boxLength")
shinyjs::disable("aquiferWidth")
updateSelectInput(session, 'mapselected_gw', choices=names(MAP_NAME_OPTIONS), selected=names(MAP_NAME_OPTIONS)[[1]])


# REACTIVE ---------------------

# Read in appropriate template file based on choice in UI
datFileLoc <- reactive({
  if (input$methodselected=="デフォルト") datFile <- "data/at123dat/TMDU_INPUT_TYPE0.DAT"
  if (input$methodselected=="マイ設定１") datFile <- "data/at123dat/TMDU_INPUT_TYPE1.DAT"
  if (input$methodselected=="マイ設定２") datFile <- "data/at123dat/TMDU_INPUT_TYPE2.DAT"
  return(datFile)
})

datData <- reactive({
    tryCatch({
        datFile <- datFileLoc()
        lines <- readLines(datFile)
        parameters <- list()
        parameters$TITLE <-  lines[1] %>% trimws
        tmp <- lines[2] %>% ff1 %>% ff2 %>% as.numeric
        parameters$noX <- tmp[1]; parameters$noY <- tmp[2]; parameters$noZ <- tmp[3]; parameters$plotControl <- tmp[4]; parameters$noBeginTS <- tmp[5]; parameters$noEndTS <- tmp[6]; parameters$noTimeIntervals <- tmp[7]; parameters$sourceControl <- tmp[8]; parameters$conditionControl <- tmp[9]; parameters$outputControl <- tmp[10]; parameters$caseControl <- tmp[11]; parameters$widthControl <- tmp[12]; parameters$depthControl <- tmp[13]; parameters$IBUG <- tmp[14]; parameters$integrationControl <- tmp[15]
        tmp<-NULL
        for (i in 1:8)  tmp[i] <- lines[3] %>% substring(i*10-9,i*10) %>% trimws %>% as.numeric
        parameters$aquiferDepth <- tmp[1]; parameters$aquiferWidth <- tmp[2]; parameters$beginX <- tmp[3]; parameters$endX <- tmp[4]; parameters$beginY <- tmp[5]; parameters$endY <- tmp[6]; parameters$beginZ <- tmp[7]; parameters$endZ <- tmp[8];
        tmp<-NULL
        for (i in 1:8)  tmp[i] <- lines[4] %>% substring(i*10-9,i*10) %>% trimws %>% as.numeric
        parameters$porosity <- tmp[1]; parameters$hydraulicCond <- tmp[2]; parameters$hydraulicGrad <- tmp[3]; parameters$longDisp <- tmp[4]; parameters$latDisp <- tmp[5]; parameters$vertDisp <- tmp[6]; parameters$distCoef <- tmp[7]; parameters$AKE <- tmp[8]
        tmp<-NULL
        for (i in 1:8)  tmp[i] <- lines[5] %>% substring(i*10-9,i*10) %>% trimws %>% as.numeric
        parameters$diffusionCoef <- tmp[1]; parameters$decayConst <- tmp[2]; parameters$bulkDen <- tmp[3]; parameters$denOfWater <- tmp[4]; parameters$errorTole <- tmp[5]; parameters$timeInterval <- tmp[6]; parameters$distTime <- tmp[7]; parameters$releaseRate <- tmp[8]
        return(parameters)
    }, error = function(err) {})
    return(NULL)
})

gwFocusOn <- reactive(input$gwFocusOn)
clickedlatlng <- reactive(input$map_click)
sourceLocation <- reactive({
    sourceLatlng <- input$sourceLatlng 
    if (sourceLatlng != '') {
        tryCatch({
            latlng <- strsplit(sourceLatlng, ' - ')[[1]]
            return(list(lat=as.numeric(latlng[1]), lng=as.numeric(latlng[2])))
        }, error = function (err) {})
    }
    return(NULL)
})

# Direction from auto-calculate or manual-input
direction <- reactive({
  direction_e <- input$direction
  sourceLatlng <- input$sourceLatlng 
  if (sourceLatlng != '') {
    tryCatch({
      if (input$ignoreAltitude == FALSE) {
        latlng <- strsplit(sourceLatlng, ' - ')[[1]]
        dat_elev <- data.table(latlng[2] %>% as.numeric,latlng[1] %>% as.numeric) %>% coord.at.d %>% get.mesh %>% get.edge.source %>% get.elevation(z=14) %>% get.best.angle %>% unlist %>% t %>% data.frame
        if (dat_elev %>% select(p.same.elevation.drop) %>% is_greater_than(75) ) message("注意: 選択した地域が75%以上 平らなため、新たに地点を選ぶか、手動で角度を設定することをお勧めします。")
        if (dat_elev %>% select(elevation.difference.max) %>% equals(0) ) message("注意: 選択した地点が最小標高であるため、新たに地点を選ぶか、手動で角度を設定することをお勧めします。")
        direction <- dat_elev %>% select(mean.heading) %>% as.numeric
        updateTextInput(session, 'direction', value = direction)
        return(list(direction = direction))
      } else {
        return(list(direction = direction_e %>% as.numeric()))
      }
    }, error = function (err) {})
  }
  return(NULL)
})

direction_info <- reactive({
  direction <- input$direction
  sourceLatlng <- input$sourceLatlng 
  tryCatch({
    latlng <- strsplit(sourceLatlng, ' - ')[[1]]
    dat_elev <- data.table(latlng[2] %>% as.numeric,latlng[1] %>% as.numeric) %>% coord.at.d %>% get.mesh %>% get.edge.source %>% get.elevation(z=14) %>% get.best.angle %>% unlist %>% t %>% data.frame
    return(list(hgrad_elev=dat_elev %>% select(source.elevation) %>% subtract(dat_elev %>% select(mean.elevation)) %>% divide_by(dat_elev %>% select(mean.dist_km) %>% multiply_by(1000)) %>% as.numeric,
                lat.d=dat_elev %>% select(mean.lat.d) %>% as.numeric,
                lng.d=dat_elev %>% select(mean.lng.d) %>% as.numeric))
  }, error = function (err) {})
  return(NULL)
})

isPlayRiver <- reactive(input$isPlayRiver)
sliceContour <- reactive(input$sliceContour)

# OBSERVE ---------------------
# Update boxLength min
observe({
    boxLength <- input$boxLength
    if (boxLength < 0) {
        updateNumericInput(session, 'boxLength', value=0)
    }
})

# Set source location
observe({
  clickedlatlng <- clickedlatlng()
  
  isolate({
    gwFocusOn <- gwFocusOn()
    if (gwFocusOn == 'sourceLatlng') {
      if (!is.null(clickedlatlng)) {
        updateTextInput(session, 'sourceLatlng', value=paste(clickedlatlng$lat, '-', clickedlatlng$lng))
        session$sendCustomMessage('onUpdateGwFocusOn', 'direction')
      }
    }
    else if (gwFocusOn == 'direction') {
      if (!is.null(clickedlatlng) && input$ignoreAltitude == TRUE) {
        updateTextInput(session, 'destinationLatlng', value=paste(clickedlatlng$lat, '-', clickedlatlng$lng))
      }
    }
  })
})

# Change Map tile
observe({
  selectedMapName <- input$mapselected_gw
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

# Draw Grid
observe({
    sourceLocation <- sourceLocation()
    boxLength <- input$boxLength
    direction <- direction()
    direction <- direction$direction
    
    # browser()
    
    if (is.null(sourceLocation)) {
        return(NULL)
    }

    isolate({
        if (input$tabs == 'groundwater') {
          session$sendCustomMessage('onDrawGrid', list(from=sourceLocation, distance=boxLength, direction=direction, noX=input$noX, noY=input$noY, settings=getGroundwaterSettings()))
          session$sendCustomMessage('onUpdateGridCoordinates', list(from=sourceLocation, distance=boxLength, direction=direction, noX=input$noX, noY=input$noY))
        }
    })
})

# Update Properties
observe({
    datData <- datData()
    direction_info <- direction_info()
    datData$hydraulicGrad  <- direction_info$hgrad_elev
    if (is.null(datData)) {
        return(NULL)
    }

    isolate({
        updateProperties(datData)
    })
})

# Update Box length
observe({
    noK <- input$noK
    aquiferWidth <- input$aquiferWidth

    isolate({
        updateNumericInput(session, 'boxLength', value=round(noK * aquiferWidth / sqrt(2)))
    })
})

# Update LegendsGW
observe({
    slice <- input$sliceContour

    isolate({
        updateCheckboxInput(session, 'isPlayRiver', value=F)
        if (!is.null(outputContour)) {
            updateLegendsGW(outputContour, input$sliceContour)
        }
    })
})

# Enable/disable contour smooth
observe({
    contourSmoothAuto <- input$contourSmoothAuto

    isolate({
        if (contourSmoothAuto) {
            shinyjs::disable("contourSmooth")
        } else {
            shinyjs::enable("contourSmooth")
        }
    })
})

# Change param by upload CSV
observe({
  paramCSV <- input$csvfile3
  isolate({
    if (is.null(paramCSV))
      return();
    dataParam <- readParam(paramCSV$datapath)
    # Update inputs
    updateNumericInput(session, 'noX', value=as.numeric(dataParam[dataParam$PARAM_NAME == "noX",]["VALUE"]))
    updateNumericInput(session, 'noY', value=as.numeric(dataParam[dataParam$PARAM_NAME == "noY",]["VALUE"]))
    updateNumericInput(session, 'porosity', value=as.numeric(dataParam[dataParam$PARAM_NAME == "porosity",]["VALUE"]))
    updateNumericInput(session, 'hydraulicCond', value=as.numeric(dataParam[dataParam$PARAM_NAME == "hydraulicCond",]["VALUE"]))
    updateNumericInput(session, 'longDisp', value=as.numeric(dataParam[dataParam$PARAM_NAME == "longDisp",]["VALUE"]))
    updateNumericInput(session, 'latDisp', value=as.numeric(dataParam[dataParam$PARAM_NAME == "latDisp",]["VALUE"]))
    updateNumericInput(session, 'vertDisp', value=as.numeric(dataParam[dataParam$PARAM_NAME == "vertDisp",]["VALUE"]))
    updateNumericInput(session, 'diffusionCoef', value=as.numeric(dataParam[dataParam$PARAM_NAME == "diffusionCoef",]["VALUE"]))
    updateNumericInput(session, 'decayConst', value=as.numeric(dataParam[dataParam$PARAM_NAME == "decayConst",]["VALUE"]))
    updateNumericInput(session, 'bulkDen', value=as.numeric(dataParam[dataParam$PARAM_NAME == "bulkDen",]["VALUE"]))
    updateNumericInput(session, 'denOfWater', value=as.numeric(dataParam[dataParam$PARAM_NAME == "denOfWater",]["VALUE"]))
    updateNumericInput(session, 'errorTole', value=as.numeric(dataParam[dataParam$PARAM_NAME == "errorTole",]["VALUE"]))
    updateNumericInput(session, 'timeInterval', value=as.numeric(dataParam[dataParam$PARAM_NAME == "timeInterval",]["VALUE"]))
    updateNumericInput(session, 'distTime', value=as.numeric(dataParam[dataParam$PARAM_NAME == "distTime",]["VALUE"]))
    updateNumericInput(session, 'releaseRate', value=as.numeric(dataParam[dataParam$PARAM_NAME == "releaseRate",]["VALUE"]))
    updateNumericInput(session, 'hydraulicGrad', value=as.numeric(dataParam[dataParam$PARAM_NAME == "hydraulicGrad",]["VALUE"])) 
    updateNumericInput(session, 'distCoef', value=as.numeric(dataParam[dataParam$PARAM_NAME == "distCoef",]["VALUE"]))
  })
})



# OBSERVE EVENT ---------------------

# Create AT123 output files with input data
observeEvent(input$calculate, {
    datFile <- datFileLoc()
    req(datFile)
    showNotification(paste0('地下水計算中…'), type="message")
    datData <- datData()
    direction <- direction()
    # direction <- direction$direction_elev
    direction_info <- direction_info()
    datData$hydraulicGrad <- direction_info$hgrad_elev
    sourceLocation <- sourceLocation()
    sliceContour <- sliceContour()
    noZ <- datData$noZ

    session$sendCustomMessage('onCalculateAT123', '')

    if (is.null(datData)) {
        showNotification(paste0('dat形式のファイルを更新してください。'), type="message")
        return(NULL)
    }
    if (is.null(sourceLocation)) {
        showNotification(paste0('漏洩源を選択してください。'), type="message")
        return(NULL)
    }
    if (!(sliceContour >= 1 && sliceContour <= noZ)) {
        showNotification(paste0('スライス値が無効です。スライス値は1以上', noZ, '以下である必要があります。'), type="message")
        return(NULL)
    }

    tryCatch({
      if (input$methodselected=="デフォルト") {
        at123.names <- c("data/at123dat/TMDU_INPUT_TYPE0.DAT", "data/at123dat/TMDU_OUTPUT_TYPE0.DAT", "data/at123dat/TMDU_CONTOUR_TYPE0.DAT") 
        # at123.names <- c("data/at123dat/TMDU_INPUT_TYPE1.DAT", "data/at123dat/TMDU_OUTPUT_TYPE1.DAT", "data/at123dat/TMDU_CONTOUR_TYPE1.DAT") 
        fix.input <- fix.input1
      }
      if (input$methodselected=="マイ設定１") {
        at123.names <- c("data/at123dat/TMDU_INPUT_TYPE1.DAT", "data/at123dat/TMDU_OUTPUT_TYPE1.DAT", "data/at123dat/TMDU_CONTOUR_TYPE1.DAT") 
        # at123.names <- c("data/at123dat/TMDU_INPUT_TYPE2.DAT", "data/at123dat/TMDU_OUTPUT_TYPE2.DAT", "data/at123dat/TMDU_CONTOUR_TYPE2.DAT") 
        fix.input <- fix.input2
      }
      if (input$methodselected=="マイ設定２") {
        at123.names <- c("data/at123dat/TMDU_INPUT_TYPE2.DAT", "data/at123dat/TMDU_OUTPUT_TYPE2.DAT", "data/at123dat/TMDU_CONTOUR_TYPE2.DAT") 
        # at123.names <- c("data/at123dat/TMDU_INPUT_TYPE3.DAT", "data/at123dat/TMDU_OUTPUT_TYPE3.DAT", "data/at123dat/TMDU_CONTOUR_TYPE3.DAT") 
        fix.input <- fix.input3
      }
      # browser()
        data <- input.list(datFile) %>% ui.input %>% fix.input %>% revert.input
        data %>% write.table(at123.names[1], row.names = F, quote = F, col.names = F)
        tryCatch({
            system2(command="data/at123dat/at123d.exe", input=at123.names)
            outputContour <<- output.list(at123.names[3], at123.names[1])
            updateLegendsGW(outputContour, input$sliceContour)
            session$sendCustomMessage('onUpdateContour', list(ui=list(
                noX=input$noX, noY=input$noY, noZ=noZ, slice=sliceContour,
                boxLength=input$boxLength, from=sourceLocation, direction=direction,
                colors=CONTOUR_COLORS
                ), data=outputContour, settings=getGroundwaterSettings()))
        }, error = function (err) {
            print(err)
            showNotification(paste0('AT123.exeの実行中にエラー発生。'), type="message")
        })
    }, error = function (err) {
        print(err)
    })
    showNotification(paste0('地下水計算(',input$methodselected,')完了。'), type="message")
    # print(datData$TITLE) # check it is reading it in correctly
})


# TAK 
# Save output 
# output$downloadat123 = downloadHandler(
# filename = "TMDU_CONTOUR.xlsx",
#   content = function(file) {
#     m0 <- length(outputContour) # 1 to 6
#     k0 <- dim(outputContour[[1]])[3] # how many in z direction
#     outputContour[[1]][,,1] %>% write.xlsx(file=file, sheetName="blank", row.names=FALSE)
#     for (m in 1:m0) {
#       for (k in 1:k0) {
#         outputContour[[m]][,,k] %>% write.xlsx(file=file, sheetName=paste0("time",m,"_z",k), append=T, row.names=FALSE)
#       }
#     }
#   }
# )

# Save AT123 contour as Excel file 
# output$downloadat123_e = downloadHandler(
#   filename = "TMDU_CONTOUR.xlsx",
#   content = function(file) {
#     wb = createWorkbook()
#     m0 <- length(outputContour) # 1 to 6
#     k0 <- dim(outputContour[[1]])[3] # how many in z direction
#     for (k in 1:k0) {
#       for (m in 1:m0) {
#         sheet = createSheet(wb, paste0("slice",k,"_time",m))
#         outputContour[[m]][,,k] %>% addDataFrame(sheet=sheet, startColumn=1, row.names=F, col.names=F)
#       }
#     }
#     saveWorkbook(wb, file = file)
#   }
# )  

# Save AT123 contour as CSV file 
output$downloadat123 = downloadHandler(
  filename = function() {
    print("Export CSV Contour: file name")
    paste('TMDU_CONTOUR.csv', sep='')
  },
  content = function(file) {
    print("Export CSV Contour: content")
    print(file)
    # wb = createWorkbook()
    m0 <- length(outputContour) # 1 to 6
    k0 <- dim(outputContour[[1]])[3] # how many in z direction
    lng.i <- nrow(outputContour[[1]])
    lat.j <- ncol(outputContour[[1]])
    contour <- NULL
    for (k in 1:k0) { # k=m=1
      for (m in 1:m0) {
        tmp <- data.frame(VALUE = outputContour[[m]][,,k] %>% c,
                          LONGITUDE = rep(1:lng.i,lat.j),
                          LATITUDE = rep(1:lat.j,each=lng.i),
                          SLICE = k,
                          TIMESTEP = m)
        contour %<>% rbind(tmp) 
      }
    }
    
    # browser()
    # Convert indices to lon/lat
    direction <- direction()
    sourceLocation <- sourceLocation()
    tmp2 <- sourceLocation %>% as.data.frame %>% select(2,1) %>% coord.at.d(d=sqrt(2)*input$boxLength/1000,a=direction) %>% get.mesh(noX=input$noX,noY=input$noY,loc="corner") 
    tmp2$oldVar1 %<>% add(1)
    tmp2$oldVar2 %<>% add(1)
    lab1 <- paste0(tmp2$oldVar1,",",tmp2$oldVar2)
    lab2 <- paste0(contour$LONGITUDE,",",contour$LATITUDE)
    j <- NULL
    for (i in 1:nrow(contour)) j[i] <- which(lab2[i]==lab1)
    contour$LONGITUDE <- tmp2$lng.d[j]
    contour$LATITUDE <- tmp2$lat.d[j]
    fwrite(contour, file=file, col.names = T)
  }
)  


# Save map as shapefile (zipped) 
# output$downloadshp = downloadHandler(
#   filename = "TMDU_CONTOUR_SHP.zip",
#   content = function(file) {
#     data = jjjj
#     temp_shp <- tempdir()
#     writeOGR(data, temp_shp, "TMDU_CONTOUR", "ESRI Shapefile", overwrite_layer = TRUE)
#     zip_file <- file.path(temp_shp, "TMDU_CONTOUR_SHP.zip")
#     shp_files <- list.files(temp_shp,"TMDU_CONTOUR",full.names = TRUE)
#     zip_command <- paste("zip -r", zip_file, paste(shp_files, collapse = " "))
#     system(zip_command)
#     file.copy(zip_file, file)
#     file.remove(zip_file, shp_files)
#   }
# )  


#write the polys to .shp
# output$downloadshp <- downloadHandler(
#   filename = 'shpExport.zip',
#   content = function(file) {
#     if (length(Sys.glob("shpExport.*"))>0){
#       file.remove(Sys.glob("shpExport.*"))
#     }
#     data = 100
#     temp_shp <- tempdir()
#     writeOGR(data, temp_shp, "TMDU_CONTOUR", "ESRI Shapefile", overwrite_layer = TRUE)
#     zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"))
#     file.copy("shpExport.zip", file)
#     if (length(Sys.glob("shpExport.*"))>0){
#       file.remove(Sys.glob("shpExport.*"))
#     }
#   }
# )


# tmp
# cartesian.m <- matrix(0,nrow = 16,ncol=16)
# # i0 <- 0
# # j0 <- 0 # reference will be 0,0
# latitude.m <- longitude.m <- azimuthal.m <- polar.m <- angle.m <- radius.m <- cartesian.m
# for (i in 1:16) {
#   for (j in 1:16) { # i=j=2
#     radius.m[i,j] <- sqrt(i^2+j^2)
#     angle.m[i,j] <- atan2(j,i)
#     polar.m[i,j] <- acos(0)
#   }
# }
# 
# lat.f <- function(r, x, y, z) return(arcsin(z/r)(180/pi))
# 
# lng.f <- function(r, x, y, z) {
#   if (x > 0) {
#     lng <- arctan(y/x)(180/pi)
#   } else if (y > 0) {
#     lng <- arctan(y/x)(180/pi) + 180
#   } else {
#     lng <- arctan(y/x)(180/pi) - 180
#   }
#   return(lng)
# }
# 
# Pto <- "St1"
# X <- 1711591.78090565
# Y <- -5060304.1659587
# Z <- -3473256.69328603
# XYZ_df <- as.data.frame(cbind(Pto, X, Y, Z))
# GeodesiCL::geodesic(5, XYZ_df, digits = 4)
