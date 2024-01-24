library(readxl)
library(tidyverse)
library(magrittr)
library(sf)
library(leaflet)


Air <- read_excel("data/大気モデルの出力データ形式_210224.xlsx", 
                  col_types = c("skip", "numeric", "skip", 
                                "numeric", "numeric", "numeric", 
                                "skip", "skip", "skip", "skip"), 
                  col_names = c("x", "AEGL1", "AEGL2", "AEGL3"),
                  skip = 4)

# 発生源位置座標（仮で名古屋市立大学を発生源としています）
st_point(x = c(136.93437794475813, 35.13896325765297)) %>%
  st_sfc(crs = 4326) %>%
  st_transform(2459) %>%
  st_coordinates() ->
  origin


# AEGL1, 2, 3のエリアを示す点座標を発生源座標に平行移動
Air %>% 
  filter(AEGL1 > 0) %$%
  tibble(x = c(0, x, rev(x), 0) + origin[1],
         y = c(0, AEGL1, rev(-AEGL1), 0) + origin[2]) ->
  points1

Air %>% 
  filter(AEGL2 > 0) %$%
  tibble(x = c(0, x, rev(x), 0) + origin[1],
         y = c(0, AEGL2, rev(-AEGL2), 0) + origin[2]) ->
  points2

Air %>% 
  filter(AEGL3 > 0) %$%
  tibble(x = c(0, x, rev(x), 0) + origin[1],
         y = c(0, AEGL3, rev(-AEGL3), 0) + origin[2]) ->
  points3


# 緯度経度に変換
st_multipoint(as.matrix(points1)) %>%
  st_sfc(crs = 2459) %>%
  st_transform(4326) %>%
  st_coordinates() ->
  coo1

st_multipoint(as.matrix(points2)) %>%
  st_sfc(crs = 2459) %>%
  st_transform(4326) %>%
  st_coordinates() ->
  coo2

st_multipoint(as.matrix(points3)) %>%
  st_sfc(crs = 2459) %>%
  st_transform(4326) %>%
  st_coordinates() ->
  coo3


# 地図で表示
leaflet() %>%
  addTiles() %>%
  addPolygons(lng = coo1[,1], lat = coo1[,2], fillColor = "blue", fillOpacity = 0.5, weight = 0, label = "AEGL1") %>%
  addPolygons(lng = coo2[,1], lat = coo2[,2], fillColor = "yellow", fillOpacity = 0.5, weight = 0, label = "AEGL2") %>%
  addPolygons(lng = coo3[,1], lat = coo3[,2], fillColor = "red", fillOpacity = 0.5, weight = 0, label = "AEGL3") %>%
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")
