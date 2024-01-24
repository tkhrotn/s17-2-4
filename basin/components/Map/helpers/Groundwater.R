# Groundwater calculation

# Internal functions ---------------
ff1 <- function(x) return(gsub("\\s+", " ", gsub("^\\s+|\\s+$", "",x)))
ff2 <- function(x) return(strsplit(x," ")[[1]])

# Input and Output functions -------------
input.list <- function(input.path) {
  # input data to list
  # lines <- readLines(input$datFile$datapath)
  lines <- readLines(input.path)
  parameters <- list()
  parameters$TITLE <-  lines[1] %>% trimws
  tmp <- lines[2] %>% ff1 %>% ff2 %>% as.numeric
  parameters$noX <- tmp[1]; 
  parameters$noY <- tmp[2]; 
  parameters$noZ <- tmp[3]; 
  parameters$plotControl <- tmp[4]; 
  parameters$noBeginTS <- tmp[5]; 
  parameters$noEndTS <- tmp[6]; 
  parameters$noTimeIntervals <- tmp[7]; 
  parameters$sourceControl <- tmp[8]; 
  parameters$conditionControl <- tmp[9]; 
  parameters$parametersputControl <- tmp[10]; 
  parameters$caseControl <- tmp[11]; 
  parameters$widthControl <- tmp[12]; 
  parameters$depthControl <- tmp[13]; 
  parameters$IBUG <- tmp[14]; 
  parameters$integrationControl <- tmp[15]
  tmp<-NULL
  for (i in 1:8)  tmp[i] <- lines[3] %>% substring(i*10-9,i*10) %>% trimws %>% as.numeric
  parameters$aquiferDepth <- tmp[1]; 
  parameters$aquiferWidth <- tmp[2]; 
  parameters$beginX <- tmp[3]; 
  parameters$endX <- tmp[4]; 
  parameters$beginY <- tmp[5]; 
  parameters$endY <- tmp[6]; 
  parameters$beginZ <- tmp[7]; 
  parameters$endZ <- tmp[8];
  tmp<-NULL
  for (i in 1:8)  tmp[i] <- lines[4] %>% substring(i*10-9,i*10) %>% trimws %>% as.numeric
  parameters$porosity <- tmp[1]; 
  parameters$hydraulicCond <- tmp[2]; 
  parameters$hydraulicGrad <- tmp[3]; 
  parameters$longDisp <- tmp[4]; 
  parameters$latDisp <- tmp[5]; 
  parameters$vertDisp <- tmp[6]; 
  parameters$distCoef <- tmp[7]; 
  parameters$AKE <- tmp[8]
  tmp<-NULL
  for (i in 1:8)  tmp[i] <- lines[5] %>% substring(i*10-9,i*10) %>% trimws %>% as.numeric
  parameters$diffusionCoef <- tmp[1]; 
  parameters$decayConst <- tmp[2];
  parameters$bulkDen <- tmp[3];
  parameters$denOfWater <- tmp[4];
  parameters$errorTole <- tmp[5];
  parameters$timeInterval <- tmp[6];
  parameters$distTime <- tmp[7]; 
  parameters$releaseRate <- tmp[8]
  tmp<-list()
  coord <- lines[-(1:5)]
  for (i in 1:length(coord)) tmp[[i]] <- coord[i] %>% ff1 %>% ff2 %>% as.numeric
  tmp %<>% unlist
  parameters$X <- tmp[1:(1+parameters$noX-1)];
  parameters$Y <- tmp[(1+parameters$noX):(1+parameters$noX+parameters$noY-1)];
  parameters$Z <- tmp[(1+parameters$noX+parameters$noY):(1+parameters$noX+parameters$noY+parameters$noZ-1)]
  return(parameters)
}

ui.input <- function(i) {
  # Function to take parameters from UI and overwrite values in input object list
  i$noX <- input$noX
  i$noY <- input$noY
  i$porosity <- input$porosity
  i$hydraulicCond <- input$hydraulicCond
  i$longDisp <- input$longDisp
  i$latDisp <- input$latDisp
  i$vertDisp <- input$vertDisp
  i$distCoef <- input$distCoef
  i$diffusionCoef  <- input$diffusionCoef
  i$decayConst  <- input$decayConst
  i$bulkDen  <- input$bulkDen
  i$denOfWater  <- input$denOfWater
  i$errorTole  <- input$errorTole
  i$timeInterval <- input$timeInterval
  i$distTime  <- input$distTime
  i$releaseRate <- input$releaseRate
  return(i)
}
# Created fix.input for three types of calculations
# fix.input <- function(i) {
#   # Function to fix any wrong values in input data
#   i$integrationControl <- 1 # must use Romberg Extrapolation
#   i$sourceControl <- 1 # continuous release
#   i$conditionControl <- 0 # must be constant source
#   i$widthControl <- 1 # not infinite in y direction
#   i$plotControl <- 2
#   i$IBUG <- 0
#   i$caseControl <- 2 # 2=chemical, 1=heat, 3=radwaste
#   return(i)
# }

fix.input1 <- function(i) {
  # Function to fix any wrong values in input data, for TYPE 1
  i$integrationControl <- 1 # must use Romberg Extrapolation
  i$sourceControl <- 1 # continuous release
  i$conditionControl <- 0 # must be constant source
  i$widthControl <- 1 # not infinite in y direction
  i$plotControl <- 2
  i$IBUG <- 0
  i$caseControl <- 2 # 2=chemical, 1=heat, 3=radwaste
  return(i)
}
fix.input2 <- function(i) {
  # Function to fix any wrong values in input data, for TYPE 2
  i$integrationControl <- 1 # must use Romberg Extrapolation
  i$sourceControl <- 1 # continuous release
  i$conditionControl <- 0 # must be constant source
  i$widthControl <- 1 # not infinite in y direction
  i$plotControl <- 2
  i$IBUG <- 0
  i$caseControl <- 2 # 2=chemical, 1=heat, 3=radwaste
  return(i)
}
fix.input3 <- function(i) {
  # Function to fix any wrong values in input data, for TYPE 3
  i$integrationControl <- 1 # must use Romberg Extrapolation
  i$sourceControl <- 1 # continuous release
  i$conditionControl <- 0 # must be constant source
  i$widthControl <- 1 # not infinite in y direction
  i$plotControl <- 2
  i$IBUG <- 0
  i$caseControl <- 2 # 2=chemical, 1=heat, 3=radwaste
  return(i)
}
revert.input <- function(input.data=i) {
  # list to input data format for exporting
  f <- function(xx) {
    gr <- as.numeric(gl(ncol(xx), 8, ncol(xx)))+3
    lst <- split(unlist(xx), gr)
    xx <- do.call(rbind, lapply(lst, `length<-`, max(lengths(lst))))
    xx[is.na(xx)] <- ""
    xx.out <- NULL
    for (j in 1:nrow(xx)) xx.out[j] <- xx[j,] %>% str_pad(width=10) %>% paste(collapse = "")
    return(xx.out)
  }
  g <- function(x,n=3) return(format(round(x, digits=10), nsmall = n)) # to add 0s after decimal (as float)
  out <- input.data$TITLE %>% toupper
  out %<>% c(input.data[-1] %>% unlist %>% t %>% as.data.frame %>% dplyr::select(1:15) %>% str_pad(width=5) %>% paste(collapse = ""),
             input.data[-1] %>% unlist %>% t %>% as.data.frame %>% dplyr::select(16:23) %>% g %>% str_pad(width=10) %>% paste(collapse = ""),
             input.data[-1] %>% unlist %>% t %>% as.data.frame %>% dplyr::select(24:31) %>% g %>% str_pad(width=10) %>% paste(collapse = ""),
             input.data[-1] %>% unlist %>% t %>% as.data.frame %>% dplyr::select(32:39) %>% g %>% str_pad(width=10) %>% paste(collapse = ""),
             input.data[-1] %>% unlist %>% t %>% as.data.frame %>% dplyr::select(-(1:39)) %>% g(2) %>% dplyr::select(1:input.data$noX) %>% f,
             input.data[-1] %>% unlist %>% t %>% as.data.frame %>% dplyr::select(-(1:39)) %>% g(2) %>% dplyr::select((input.data$noX+1):(input.data$noX+input.data$noY)) %>% f,
             input.data[-1] %>% unlist %>% t %>% as.data.frame %>% dplyr::select(-(1:39)) %>% g(2) %>% dplyr::select((input.data$noY+input.data$noX+1):(input.data$noY+input.data$noX+input.data$noZ)) %>% f)
  for (k in 1:length(out)) out[k] %<>% toupper # fix exponent from e to E
  return(out)
}

output.list <- function(output.name="TMDU_CONTOUR.DAT",input.name="TMDU_INPUT.DAT") {
  # create array from output TEST.CON file
  input.file <- input.list(input.name)
  output.file <- readLines(output.name, warn = F)
  t <- which(output.file %>% substring(1,9)=="1    DIST") %>% c(length(output.file)) # get rows that start with this string, and last row
  out <- list()
  for (m in 1:(input.file$noEndTS+1)) out[[m]] <- readLines(output.name, warn = F)[(t[m]+1):(t[m+1]-1)] %>% paste(collapse="") %>% ff1 %>% ff2 %>% as.numeric %>% array(dim=c(input.file$noX,input.file$noY,input.file$noZ)) #%>% aperm(c(2,1,3))
  return(out)
}

# Plotting functions -----------------
cf_grid_ <- function(x) return(cf_grid(x,with_lines=T, main="", color.palette = gw.palette, axes=F))

gw.palette <- function (n, name = c("beach.colors")) {
  # create own palette
  r <- c(76, 0, 0, 0, 0, 255, 255, 255, 255, 230, 209, 189, 168, 148, 128, 107, 87, 66, 46, 26)
  g <- c(0, 67, 158, 211, 229, 237, 243, 249, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255)
  b <- c(255, 255, 255, 255, 255, 36, 24, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  beach.colors = rgb(r,g,b,maxColorValue = 255)
  name = match.arg(name)
  orig = eval(parse(text = name))
  rgb = t(col2rgb(orig))
  temp = matrix(NA, ncol = 3, nrow = n)
  x = seq(0, 1, , length(orig))
  xg = seq(0, 1, , n)
  for (k in 1:3) {
    hold = spline(x, rgb[, k], n = n)$y
    hold[hold < 0] = 0
    hold[hold > 255] = 255
    temp[, k] = round(hold)
  }
  palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
  palette
}



# Read .dat file
readDatFile <- function (filePath) {
    tryCatch({
        datData <- read.table(filePath, header=TRUE, sep="\t")
        return(datData)
    }, error = function (err) {})
    reset('datFile')
    return(NULL)
}

#-----------------
# Altitude related functions
#-----------------
# 1. coord.at.d
# 2. get.mesh
# 3. get.edge
# 4. get.edge.source (unused)
# 5. get.elevation
# 6. get.best.angle
# 7. plot.best.angle

# 1.Get coordinate at d from source (d and heading always fixed, and not given)
coord.at.d <- function(dat,d=1,a=45) {
  # dat contains origin longitude/latitude/heading (0 is north)/distance d (km), and whatever else
  dat %<>% cbind(d,a)
  names(dat) <- c("lon","lat","dist_km","heading")
  if (dat$lat[1] > 90) stop("Longitude and Latitude backwards?")
  if (ncol(dat)>4) names(dat)[5:ncol(dat)] <- paste0("V",5:ncol(dat))
  pts <- dat %>% 
    st_as_sf(coords = c("lon","lat")) %>%
    st_set_crs(4326) %>%
    st_transform(3095) # st_transform(32632) # UTM zone for EUROPE -> Japan
  pts$utm_n <- st_coordinates(pts)[,1]
  pts$utm_e <- st_coordinates(pts)[,2]
  deg2rad <- function(deg) {(deg * pi) / (180)}
  pts %<>% mutate(newp_e=utm_e + (dist_km*1000*cos(deg2rad(heading))), newp_n=utm_n + (dist_km*1000*sin(deg2rad(heading)))) %>% 
    data.table %>% 
    st_as_sf(coords = c("newp_n", "newp_e")) %>%
    st_set_crs(3095) %>% # st_set_crs(32632)
    st_transform(4326) %>%
    st_geometry %>% 
    unlist %>% 
    matrix(ncol=2, byrow=T) %>% 
    as.data.frame
  dat$lng.d <- pts %>% select(V1) %>% unlist 
  dat$lat.d <- pts %>% select(V2) %>% unlist 
  return(dat %>% as.data.frame) # %>% select(-dist_km, -heading))
}

# 2. Get coordinates around center or from corner
get.mesh <- function(dat, noX=15, noY=15, loc="center") {
  # use coord.at.d first, then determine coordinates for all within the mesh
  # loc = corner or center: source location
  
  # Warnings, Errors 
  if (loc=="center" & (noX%%2==0 | noY%%2==0)) stop("noX or noY is even. Change to odd number.")
  if (loc=="center" & (noX<7 | noY<7)) warning("noX or noY should be 7 or greater for best results.")
  if (loc=="corner" & (noX<3 | noY<3)) warning("noX or noY should be 3 or greater.")
  if (loc=="corner" & (noX>16 | noY>16)) stop("noX or noY is too large. Maximum 16.")
  
  
  dat %<>% slice(rep(1:n(), each = noX*noY)) %>% cbind(expand.grid(0:(noX-1),0:(noY-1)))
  dat$x.i <- dat$Var1 %>% divide_by(noX-1)
  dat$y.j <- dat$Var2 %>% divide_by(noY-1)
  dat$oldVar1 <- dat$Var1
  dat$oldVar2 <- dat$Var2
  if (loc=="center") { # shift cell points to center, shift heading, change d?
    dat$Var1 %<>% subtract((noX-1)/2)
    dat$Var2 %<>% subtract((noY-1)/2)
    dat$x.i %<>% subtract((noX-1)/2/(noX-1))
    dat$y.j %<>% subtract((noY-1)/2/(noY-1))
  }
  
  dat$lat.d <- dat$lat+dat$y.j*(dat$lat.d-dat$lat)
  dat$lng.d <- dat$lon+dat$x.i*(dat$lng.d-dat$lon)
  dat$r <- sqrt(dat$x.i^2+dat$y.j^2)/sqrt(2)
  dat$a <- atan((dat$x.i)/(dat$y.j)) %>% multiply_by(180) %>% divide_by(pi)
  dat$a[dat$r==0] <- 0
  dat$a[dat$y.j<0] <- dat$a[dat$y.j<0] + 180 # adjust angles, if y<0
  dat$a[dat$x.i<0 & dat$y.j>=0] <- dat$a[dat$x.i<0 & dat$y.j>=0] + 360 # adjust angles, if x<0, y>0
  dat$a[dat$a>180] <- dat$a[dat$a>180] - 360 # adjust angles to match those of at123
  
  dat %<>% mutate(dist_km=r, heading=a) %>% select(-r,-a,-x.i,-y.j) 
  return(dat)
}

# 3. Grab outer coordinates along mesh (for getting altitude there) + source
get.edge <- function(dat) return(dat %>% filter(Var1 %in% (dat$Var1 %>% range) | Var2 %in% (dat$Var2 %>% range)))

# 4. Grab outer coordinates along mesh (for getting altitude there) + source
get.edge.source <- function(dat) {
  out <- bind_rows(dat %>% filter(Var1==0, Var2==0),
                   dat %>% filter(Var1 %in% (dat$Var1 %>% range) | Var2 %in% (dat$Var2 %>% range)))
  return(out)
}

# 5. Get elevation of each point 
get.elevation <- function(dat,z=14) {
  dat$elevation <- dat %>% select(lng.d,lat.d) %>% get_elev_point(prj = "EPSG:4326", src="aws",z=z) %>% as.data.frame %>% select(elevation) %>% unlist %>% suppressMessages()
  return(dat)
}


# 6. Get "best" angle from source to edge, based on edges of grid 
get.best.angle <- function(dat0) { #}, map=F) {
  out <- list()
  elev0 <- dat0$elevation[1]
  elev <- dat0$elevation
  
  i <- which((elev - elev0) == min(elev - elev0))
  if (length(i)==1 & 1 %in% i) warning("Source is lowest elevation. Angle may not be accurate.") # stop("Source is lowest elevation. No angle estimated.")
  s <- dat0[i,]
  out$dat0 <- dat0
  out$n.same.elevation.drop <- nrow(s)
  out$n.elevation.all <- nrow(dat0)
  out$p.same.elevation.drop <- 100*nrow(s)/nrow(dat0)
  
  the.mean <- dat0[i,] %>% apply(2,mean) %>% t %>%  as.data.frame
  the.mean$heading <- atan(the.mean$Var1/the.mean$Var2) %>% multiply_by(180) %>% divide_by(pi)
  the.mean$heading[the.mean$Var2<0] <- the.mean$heading[the.mean$Var2<0] + 180 # adjust angles, if y<0
  the.mean$heading[the.mean$heading>180] <- the.mean$heading[the.mean$heading>180] -360 # adjust angles to match those of at123
  out$source.elevation <- elev0
  out$elevation.difference.max <- abs(min(elev - elev0))
  out$mean <- the.mean
  return(out) # return values and do not plot
}

# 7. Plot on map "best" angle from source to edge, based on edges of grid 
plot.best.angle <- function(dat0) {
  dat0 <- dat0$dat0
  elev0 <- dat0$elevation[1]
  elev <- dat0$elevation
  i <- which((elev - elev0) == min(elev - elev0))
  if (length(i)==1 & 1 %in% i) stop("Source is lowest elevation. No angle estimated.")
  s <- dat0[i,]
  the.mean <- dat0[i,] %>% apply(2,mean) %>% t %>%  as.data.frame
  the.mean$heading <- atan(the.mean$Var1/the.mean$Var2) %>% multiply_by(180) %>% divide_by(pi)
  the.mean$heading[the.mean$Var2<0] <- the.mean$heading[the.mean$Var2<0] + 180 # adjust angles, if y<0
  the.mean$heading[the.mean$heading>180] <- the.mean$heading[the.mean$heading>180] -360 # adjust angles to match those of at123
  dat0 %>% leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.EPSG3857"))) %>% addTiles() %>%
    addCircleMarkers(lng = ~lon[1], lat = ~lat[1], col="black", opacity=1, radius=3) %>% # source
    addCircleMarkers(lng = ~lng.d, lat = ~lat.d, col="blue") %>% # points 
    addCircleMarkers(lng = ~lng.d[i], lat = ~lat.d[i], col="red",opacity = 1) %>% # all possibilities
    addCircleMarkers(lng = the.mean$lng.d, lat = the.mean$lat.d, col="green",opacity = 1) # mean
}

## Example usage
# dat <- data.table(c(135.47962531686105, # by mountain
#                     135.5897576591972, # by river
#                     138.72754031015168, # Mt Fuji
#                     136.235941185112), # random
#                   c(34.839310698220366,
#                     34.78644090606354,
#                     35.36142754740843,
#                     35.9999))
### dat %>% slice(2) %>% coord.at.d %>% get.mesh %>% get.edge.source %>% get.elevation(z=10) %>% get.best.angle %>% plot.best.angle
# dat %>% slice(2) %>% coord.at.d %>% get.mesh %>% get.elevation(z=9) %>% get.best.angle %>% plot.best.angle

# 
initialMap <- function () {
    LMap <- leaflet(list()) %>%
            addTiles(basemap, attribution = til) %>%  
            addMiniMap(toggleDisplay = TRUE, position = "bottomleft") %>%
            leaflet::addScaleBar(position = "bottomright") %>%
            addPolygons(
                data = ws_population, group="給水人口", fillColor = ~pal_ws_population(ws_population$P21A_004r), stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5
            ) %>%
            addCircleMarkers(
                lng = ~wells$X, lat = ~wells$Y, radius = 5., color = "cyan", dashArray = "2", label = labels_wells, group = "水道用井戸",
                labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "水道用井戸")
            ) %>%
            addCircleMarkers(
                lng = ~sewage_facility$X, lat = ~sewage_facility$Y, radius = 5., color = "green", dashArray = "2", label = labels_sewage_facility, group = "下水施設",
                labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "下水施設")
            ) %>%
            addCircleMarkers(
                lng = water_supply$X, lat = water_supply$Y, radius = 5., color = "#ffa500", dashArray = "2", label = labels_watersupply, group = "上水施設",
                labelOptions = labelOptions(noHide = F, direction = "bottom", style = CIRCLE_STYLES, layerId = "上水施設")
            ) %>%
            addCircleMarkers(
                lng = sluice_gate$lng, lat = sluice_gate$lat, radius = 5., color = "blue", dashArray = "2", label = "取水口", group = "取水口",
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
    output$map <- renderLeaflet(LMap)
}

# Update properties for .dat file
updateProperties <- function (datData) {
  updateNumericInput(session, 'noX', value=datData$noX)
  updateNumericInput(session, 'noY', value=datData$noY)
  updateNumericInput(session, 'porosity', value=datData$porosity)
  updateNumericInput(session, 'hydraulicCond', value=datData$hydraulicCond)
  updateNumericInput(session, 'longDisp', value=datData$longDisp)
  updateNumericInput(session, 'latDisp', value=datData$latDisp)
  updateNumericInput(session, 'vertDisp', value=datData$vertDisp)
  updateNumericInput(session, 'hydraulicGrad', value=datData$hydraulicGrad)
  updateNumericInput(session, 'distCoef', value=datData$distCoef)
  updateNumericInput(session, 'diffusionCoef', value=datData$diffusionCoef)
  updateNumericInput(session, 'decayConst', value=datData$decayConst)
  updateNumericInput(session, 'bulkDen', value=datData$bulkDen)
  updateNumericInput(session, 'denOfWater', value=datData$denOfWater)
  updateNumericInput(session, 'errorTole', value=datData$errorTole)
  updateNumericInput(session, 'timeInterval', value=datData$timeInterval)
  updateNumericInput(session, 'distTime', value=datData$distTime)
  updateNumericInput(session, 'releaseRate', value=datData$releaseRate)
  updateNumericInput(session, 'aquiferWidth', value=datData$aquiferWidth)
}

updateLegendsGW <- function (contourData, slice) {
  if (length(contourData)) {
    LMap <- leafletProxy(mapId = "map")
    LMap %>%
      removeControl(layerId = "legend_contour")

    currentSliceContourData <- c()
    for (i in 1:length(contourData)) {
      tData <- contourData[[i]]
      sliceData <- lapply(seq(dim(tData)[3]), function(x) tData[ , , x])
      if (slice >= 1 && slice <= length(sliceData)) {
        currentSliceContourData <- append(currentSliceContourData, sliceData[[slice]])
      }
    }

    if (length(currentSliceContourData) != 0) {
      minCT <- min(currentSliceContourData, na.rm = TRUE)
      maxCT <- max(currentSliceContourData, na.rm = TRUE)
      if (is.na(minCT)) minCT = 0
      if (is.na(maxCT)) maxCT = 1
      palCT <- colorBin(CONTOUR_COLORS, domain = c(minCT, maxCT), bins=length(CONTOUR_COLORS), na.color = "#808080", pretty=F)
      LMap %>% leaflet::addLegend(pal = palCT,  values = currentSliceContourData,  opacity = 0.7,  title = "Concentration(mg/L)", position = "topleft",  layerId = "legend_contour")
    }
  }
}

getGroundwaterSettings <- function () {
  return(list(
    gridTransparency=input$gridTransparency,
    gridColor=input$gridColor,
    gridWeight=input$gridWeight,
    contourSmoothAuto=input$contourSmoothAuto,
    contourSmooth=input$contourSmooth,
    contourOpacity=input$contourOpacity,

    arrowOpacity=input$arrowOpacity,
    arrowColor=input$arrowColor,
    arrowWeight=input$arrowWeight,
    arrowLength=input$arrowLength,
    sourceLocationColor=input$sourceLocationColor,
    sourceLocationOpacity=input$sourceLocationOpacity,
    sourceLocationRadius=input$sourceLocationRadius
  ))
}

updateGroundwaterSettings <- function() {
  gridTransparency <<- input$gridTransparency
  gridColor <<- input$gridColor
  gridWeight <<- input$gridWeight
  contourSmoothAuto <<- input$contourSmoothAuto
  contourSmooth <<- input$contourSmooth
  contourOpacity <<- input$contourOpacity

  arrowOpacity <<- input$arrowOpacity
  arrowColor <<- input$arrowColor
  arrowWeight <<- input$arrowWeight
  arrowLength <<- input$arrowLength
  sourceLocationColor <<- input$sourceLocationColor
  sourceLocationOpacity <<- input$sourceLocationOpacity
  sourceLocationRadius <<- input$sourceLocationRadius

  datData <- datData()

  if (!is.null(datData)) {
    session$sendCustomMessage('onDrawGrid', list(from=sourceLocation(), distance=input$boxLength, direction=input$direction, noX=input$noX, noY=input$noY, settings=getGroundwaterSettings()))
    session$sendCustomMessage('onUpdateContour', list(ui=list(
        noX=input$noX, noY=input$noY, noZ=datData$noZ, slice=input$sliceContour,
        boxLength=input$boxLength, from=sourceLocation(), direction=input$direction,
        colors=CONTOUR_COLORS
        ), redraw=T, settings=getGroundwaterSettings()))
  }
}