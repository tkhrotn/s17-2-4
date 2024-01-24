# Step 2 - River simulation as a layer
# v test code to see if it works
# - edit CTAT.R files to add new layer
# - add two toggles to show these two layers of river and air simulation results
# - when toggled on, read in original data
# - then calculation will be done to obtain the contour and cells
# - then contour and cells will show at the specified locations
# - add legend if needed
# - when toggle is on, automatically zoom into where the contour or cells are

# Some info about data:
# layer1: 濃度0のメッシュ
# layer2: 濃度0 - 0.1e-02のメッシュ
# layer3: 濃度0.1e-02 - 0.1e-01のメッシュ
# layer4: 濃度0.1e-01 - 0.1e+00のメッシュ 

# Packages
pacman::p_load(tidyverse, sf, readxl, leaflet, magrittr)
Sys.setlocale("LC_ALL", "English_United States.932")
# Read in data
Air <- fread("data/riversim/atmos_210224.csv", encoding = "UTF-8", col.names = c("x", "AEGL1", "AEGL2", "AEGL3"))
# Air <- read_excel("data/riversim/大気モデルの出力データ形式_210224.xlsx", 
#                   col_types = c("skip", "numeric", "skip", "numeric", "numeric", "numeric", "skip", "skip", "skip", "skip"), 
#                   col_names = c("x", "AEGL1", "AEGL2", "AEGL3"), skip = 4)
layers <- st_layers("data/riversim/⑤水域kmlデータ.kml")
layer1 <- st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[1])
layer2 <- st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[2])
layer3 <- st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[3])
layer4 <- st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[4])

# 発生源位置座標（仮で名古屋市立大学を発生源としています）
origin <- st_point(x = c(136.93437794475813, 35.13896325765297)) %>% st_sfc(crs = 4326) %>% st_transform(2459) %>% st_coordinates()

# AEGL1, 2, 3のエリアを示す点座標を発生源座標に平行移動
points1 <- Air %>% filter(AEGL1 > 0) %$% tibble(x = c(0, x, rev(x), 0) + origin[1], y = c(0, AEGL1, rev(-AEGL1), 0) + origin[2])
points2 <- Air %>% filter(AEGL2 > 0) %$% tibble(x = c(0, x, rev(x), 0) + origin[1], y = c(0, AEGL2, rev(-AEGL2), 0) + origin[2])
points3 <- Air %>% filter(AEGL3 > 0) %$% tibble(x = c(0, x, rev(x), 0) + origin[1], y = c(0, AEGL3, rev(-AEGL3), 0) + origin[2])
  
# 緯度経度に変換
coo1 <- st_multipoint(as.matrix(points1)) %>% st_sfc(crs = 2459) %>% st_transform(4326) %>% st_coordinates()
coo2 <- st_multipoint(as.matrix(points2)) %>% st_sfc(crs = 2459) %>% st_transform(4326) %>% st_coordinates()
coo3 <- st_multipoint(as.matrix(points3)) %>% st_sfc(crs = 2459) %>% st_transform(4326) %>% st_coordinates()

# 地図で表示, river
leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 0, color = NULL, fillColor = "blue", fillOpacity = 0.8, data = st_zm(st_geometry(layer2))) %>%
  addPolygons(weight = 0, color = NULL, fillColor = "cyan", fillOpacity = 0.8, data = st_zm(st_geometry(layer3))) %>%
  addPolygons(weight = 0, color = NULL, fillColor = "#00ff00", fillOpacity = 0.8, data = st_zm(st_geometry(layer4)))

# 地図で表示, air
leaflet() %>%
  addTiles() %>%
  addPolygons(lng = coo1[,1], lat = coo1[,2], fillColor = "blue", fillOpacity = 0.5, weight = 0, label = "AEGL1") %>%
  addPolygons(lng = coo2[,1], lat = coo2[,2], fillColor = "yellow", fillOpacity = 0.5, weight = 0, label = "AEGL2") %>%
  addPolygons(lng = coo3[,1], lat = coo3[,2], fillColor = "red", fillOpacity = 0.5, weight = 0, label = "AEGL3") %>%
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")
