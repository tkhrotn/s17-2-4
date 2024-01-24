# oct 21
srcDist <- function() {
  # calculate geodesic distance between mouse click and stations
  map <- map()
  srclat <- srclat()
  srclon <- srclon()
  d <- map %>% 
    as.data.frame %>% 
    rename(x=POINT_X, y=POINT_Y) %>% 
    extract(c("x", "y")) %>% 
    geodist(data.frame(x=srclng,y=srclat),measure="geodesic") %>% c
  return(d)
}


# Oct 20
observe({
  # Reactive display of river discharge & concentration
  map <- map()
  data <- data()
  srclat <- srclat()
  srclng <- srclng()
  sec2day <- 86400
  maxt <- maxtime() * sec2day
  mass <- releasedmass()
  area <- sectionarea()
  disp <- dispersion() #* sec2day
  velo <- rivervelocity() #* sec2day
  gamm <- skewness()
  
  getSolc_d <- function(sec2day, maxt, mass, area, disp, velo, gamm, map, srclng, srclat) {
    d <- dist_src()
    tim <- seq(0, maxt, sec2day)
    out <- data.frame(matrix(NA,length(d),length(tim)))[1,] # 23 (distances) x 21 (time 20days +1)
    for (m in 1:23) { # m=1; k=1
      k = 1
      dist <- d[m]
      for (l in tim) { # l=tim[1] L=3
        mbya <- mass/area
        val1 <- mbya / sqrt(4 * pi * disp * l)
        val2 <- -1.0 * (dist - velo * l) * (dist - velo * l) / (4 * disp * l)
        hfnc <- hermite(3, kind = "h")
        z    <- (dist - velo * l) / sqrt(2 * disp * l)
        vtmp <- data.frame(1, as.function(hfnc)(z)) %>%  suppressMessages()
        val3 <- 1 - (gamm * vtmp)
        out[,k] <- val1 * exp(val2) * val3[1, 1]
        k <- k + 1
      }
    }
    return(out)
  }

  

    getSolc_d(sec2day=86400, maxt=maxt, mass=mass, area=area, disp=disp, velo=velo, gamm=gamm,map=map, srclng=srclng, srclat=srclat)
  
  if (is.null(map()) | is.null(data())) {
    return(NULL)
  }
  
  if (input$mapselected == "Google Satellite") {
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

#-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-#
#-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-#
library(geodist)
library(magrittr)
n <- 50
# Default "cheap" distance measure is only accurate for short distances:
x <- cbind (runif (n, -0.1, 0.1), runif (n, -0.1, 0.1))
y <- cbind (runif (2 * n, -0.1, 0.1), runif (2 * n, -0.1, 0.1))
colnames (x) <- colnames (y) <- c ("x", "y")
x %>% head
y %>% head
d0 <- geodist (x) # A 50-by-50 matrix
d1 <- geodist (x, y) # A 50-by-100 matrix
d2 <- geodist (x, sequential = TRUE) # Vector of length 49
d2 <- geodist (x, sequential = TRUE, pad = TRUE) # Vector of length 50
d0_2 <- geodist (x, measure = "geodesic") # nanometre-accurate version of d0

x[1,] <- c(10,20)
x[2,] <- c(75,44)
# x[1,] <- c(20,0)
# x[2,] <- c(85,0)

d0 <- geodist (x) # A 50-by-50 matrix
d0_1 <- geodist (x, measure = "haversine") # nanometre-accurate version of d0
d0_2 <- geodist (x, measure = "vincenty") # nanometre-accurate version of d0
d0_3 <- geodist (x, measure = "geodesic") # nanometre-accurate version of d0

d0[2]/1000 # bad
d0_1[2]/1000
d0_2[2]/1000
d0_3[2]/1000

# distance between lat/lon and points on river 