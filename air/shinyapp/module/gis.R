library(RColorBrewer)
library(gstat)
library(maptools)
library(rgdal)
library(sp)
library(raster)

circle.palette <- c(rgb(255/255, 255/255, 255/255),
                    rgb(255/255, 242/255, 242/255),
                    rgb(255/255, 217/255, 217/255),
                    rgb(255/255, 191/255, 191/255),
                    rgb(255/255, 166/255, 166/255),
                    rgb(255/255, 140/255, 140/255),
                    rgb(255/255, 115/255, 115/255),
                    rgb(255/255,  89/255,  89/255),
                    rgb(255/255,  64/255,  64/255),
                    rgb(255/255,  38/255,  38/255),
                    rgb(255/255,   0/255,   0/255))

pred.palette <- c(rgb(255/255, 0/255, 0/255,   0/255),
                  rgb(255/255, 0/255, 0/255,  38/255),
                  rgb(255/255, 0/255, 0/255,  64/255),
                  rgb(255/255, 0/255, 0/255,  89/255),
                  rgb(255/255, 0/255, 0/255, 115/255),
                  rgb(255/255, 0/255, 0/255, 140/255),
                  rgb(255/255, 0/255, 0/255, 166/255),
                  rgb(255/255, 0/255, 0/255, 191/255),
                  rgb(255/255, 0/255, 0/255, 217/255),
                  rgb(255/255, 0/255, 0/255, 242/255),
                  rgb(255/255, 0/255, 0/255, 255/255))

dosage.unit <- "micro Sv/h"
data.bins <- c(1.0, 2.5, 5.0, 7.5, 10.0, 25.0, 50.0, 75.0, 100.0, 125.0)
data.labels <- c(sprintf("- %2.1f %s", data.bins[1], dosage.unit),
                 sprintf("- %2.1f %s", data.bins[2:length(data.bins)], dosage.unit),
                 sprintf("%2.1f - %s", data.bins[length(data.bins)], dosage.unit))

racdaOrdinaryKriging <- function(selected.data, resolution, arrow.log=FALSE) {
  # NAは除外
  selected.data <- selected.data[!is.na(selected.data$value),]

  if (nrow(selected.data) <= 1) 
    return(NULL)
  
  lng.min <- min(selected.data$longitude)
  lng.max <- max(selected.data$longitude)
  lat.min <- min(selected.data$latitude)
  lat.max <- max(selected.data$latitude)
  
  pj <- "+init=epsg:4612"
  
  lng <- seq(lng.min, lng.max, length.out = 2) 
  lat <- seq(lat.min, lat.max, length.out = 2)
  grd <- data.frame(lng, lat)
  coordinates(grd) <- c("lng", "lat")
  proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
  gridded(grd) <- TRUE
  
  if (arrow.log) {
    selected.data$value <- log(selected.data$value)
  }
  coordinates(selected.data) <- c("longitude", "latitude")
  proj4string(selected.data) <- CRS("+proj=longlat +datum=WGS84")

  selected.data <- spTransform(selected.data, CRS(pj))
  grd <- spTransform(grd, CRS(pj))
  selected.data <- remove.duplicates(selected.data)
  vari <- variogram(value ~ 1, selected.data)
  if(is.null(vari))
    return(NULL)
  vari.fit <- fit.variogram(vari, model=vgm("Sph"))
  gOK <- gstat(id = "value", form = value ~ 1, data = selected.data, model = vari.fit)
  
  grd <- raster(grd)
  res(grd) <- min(xmax(grd) - xmin(grd), ymax(grd) - ymin(grd)) / resolution
  raster.image <- interpolate(grd, gOK)
  raster.image <- projectRaster(raster.image, crs = CRS("+proj=longlat +datum=WGS84"))

  if (arrow.log) {
    values(raster.image) <- exp(values(raster.image))
  }
  
  return(raster.image)
}

racdaSimpleKriging <- function(selected.data, resolution, arrow.log=FALSE) {
  # NAは除外
  selected.data <- selected.data[!is.na(selected.data$value),]
  
  if (nrow(selected.data) <= 1) 
    return(NULL)
  
  lng.min <- min(selected.data$longitude)
  lng.max <- max(selected.data$longitude)
  lat.min <- min(selected.data$latitude)
  lat.max <- max(selected.data$latitude)
  
  pj <- "+init=epsg:4612"
  
  lng <- seq(lng.min, lng.max, length.out = 2) 
  lat <- seq(lat.min, lat.max, length.out = 2)
  grd <- data.frame(lng, lat)
  coordinates(grd) <- c("lng", "lat")
  proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
  gridded(grd) <- TRUE
  
  if (arrow.log) {
    selected.data$value <- log(selected.data$value)
  }
  coordinates(selected.data) <- c("longitude", "latitude")
  proj4string(selected.data) <- CRS("+proj=longlat +datum=WGS84")
  
  selected.data <- spTransform(selected.data, CRS(pj))
  grd <- spTransform(grd, CRS(pj))
  selected.data <- remove.duplicates(selected.data)
  vari <- variogram(value ~ 1, selected.data)
  if(is.null(vari))
    return(NULL)
  vari.fit <- fit.variogram(vari, model=vgm("Sph"))
  gOK <- gstat(id = "value", form = value ~ 1, data = selected.data, model = vari.fit, beta = mean(selected.data$value))
  
  grd <- raster(grd)
  res(grd) <- min(xmax(grd) - xmin(grd), ymax(grd) - ymin(grd)) / resolution
  raster.image <- interpolate(grd, gOK)
  raster.image <- projectRaster(raster.image, crs = CRS("+proj=longlat +datum=WGS84"))
  
  if (arrow.log) {
    values(raster.image) <- exp(values(raster.image))
  }
  
  return(raster.image)
}

racdaIDW <- function(selected.data, resolution, arrow.log=FALSE) {
  # NAは除外
  selected.data <- selected.data[!is.na(selected.data$value),]
  
  if (nrow(selected.data) <= 1) 
    return(NULL)
  
  lng.min <- min(selected.data$longitude)
  lng.max <- max(selected.data$longitude)
  lat.min <- min(selected.data$latitude)
  lat.max <- max(selected.data$latitude)
  
  pj <- "+init=epsg:4612"
  
  lng <- seq(lng.min, lng.max, length.out = 2) 
  lat <- seq(lat.min, lat.max, length.out = 2)
  grd <- data.frame(lng, lat)
  coordinates(grd) <- c("lng", "lat")
  proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
  gridded(grd) <- TRUE
  
  if (arrow.log) {
    selected.data$value <- log(selected.data$value)
  }
  coordinates(selected.data) <- c("longitude", "latitude")
  proj4string(selected.data) <- CRS("+proj=longlat +datum=WGS84")
  
  selected.data <- spTransform(selected.data, CRS(pj))
  grd <- spTransform(grd, CRS(pj))
  selected.data <- remove.duplicates(selected.data)
  gidw <- gstat(id = "value", form = value ~ 1, data = selected.data, nmax = 5)
  
  grd <- raster(grd)
  res(grd) <- min(xmax(grd) - xmin(grd), ymax(grd) - ymin(grd)) / resolution
  raster.image <- interpolate(grd, gidw)
  raster.image <- projectRaster(raster.image, crs = CRS("+proj=longlat +datum=WGS84"))
  
  if (arrow.log) {
    raster::values(raster.image) <- exp(raster::values(raster.image))
  }
  
  return(raster.image)
}


racdaNearestNeighbour <- function(selected.data, resolution, arrow.log=FALSE) {
  # NAは除外
  selected.data <- selected.data[!is.na(selected.data$value),]
  
  if (nrow(selected.data) <= 1) 
    return(NULL)
  
  lng.min <- min(selected.data$longitude)
  lng.max <- max(selected.data$longitude)
  lat.min <- min(selected.data$latitude)
  lat.max <- max(selected.data$latitude)
  
  pj <- "+init=epsg:4612"
  
  lng <- seq(lng.min, lng.max, length.out = 2) 
  lat <- seq(lat.min, lat.max, length.out = 2)
  grd <- data.frame(lng, lat)
  coordinates(grd) <- c("lng", "lat")
  proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
  gridded(grd) <- TRUE
  
  if (arrow.log) {
    selected.data$value <- log(selected.data$value)
  }
  coordinates(selected.data) <- c("longitude", "latitude")
  proj4string(selected.data) <- CRS("+proj=longlat +datum=WGS84")
  
  selected.data <- spTransform(selected.data, CRS(pj))
  grd <- spTransform(grd, CRS(pj))
  selected.data <- remove.duplicates(selected.data)
  gidw <- gstat(id = "value", form = value ~ 1, data = selected.data, nmax = 1, set=list(idp=0))
  
  grd <- raster(grd)
  res(grd) <- min(xmax(grd) - xmin(grd), ymax(grd) - ymin(grd)) / resolution
  raster.image <- interpolate(grd, gidw)
  raster.image <- projectRaster(raster.image, crs = CRS("+proj=longlat +datum=WGS84"))
  
  if (arrow.log) {
    raster::values(raster.image) <- exp(raster::values(raster.image))
  }
  
  return(raster.image)
}
