library(tidyverse)
library(sf)
library(readxl)
library(leaflet)


layers <- st_layers("data/⑤水域kmlデータ.kml")

# layer1: 濃度0のメッシュ
# layer2: 濃度0 - 0.1e-02のメッシュ
# layer3: 濃度0.1e-02 - 0.1e-01のメッシュ
# layer4: 濃度0.1e-01 - 0.1e+00のメッシュ 
layer1 <- st_read("data/⑤水域kmlデータ.kml", layer = layers$name[1])
layer2 <- st_read("data/⑤水域kmlデータ.kml", layer = layers$name[2])
layer3 <- st_read("data/⑤水域kmlデータ.kml", layer = layers$name[3])
layer4 <- st_read("data/⑤水域kmlデータ.kml", layer = layers$name[4])

# 地図で表示
leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 0, color = NULL, fillColor = "blue", fillOpacity = 0.8, data = st_zm(st_geometry(layer2))) %>%
  addPolygons(weight = 0, color = NULL, fillColor = "cyan", fillOpacity = 0.8, data = st_zm(st_geometry(layer3))) %>%
  addPolygons(weight = 0, color = NULL, fillColor = "#00ff00", fillOpacity = 0.8, data = st_zm(st_geometry(layer4)))
  
  
