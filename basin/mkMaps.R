pacman::p_load(readr,readxl,xts,magrittr,sf)

# WATER SUPPLIED POPULATION (polygons)
ws_population <- geojsonio::geojson_read('data/polygons/P21-12a_27.geojson', what = "sp")
ws_population$P21A_004r <- ws_population$P21A_004/1000.0
pal_ws_population <- colorBin("RdYlBu", domain = ws_population$P21A_004r, reverse = TRUE, bins = 6)

# WATER SUPPLIED FACILITIES (points)
water_supply <- read.csv("data/static-data/water_supplu_H24(H22).csv", encoding = "UTF-8")
ws_name <- water_supply[,3]
ws_maxm <- water_supply[,4]
labels_watersupply <- sprintf("%s: %g", ws_name, ws_maxm) %>% lapply(htmltools::HTML)

# 水道用井戸（水系調査図）
wells <- read.csv("data/static-data/suidoyoido.csv")
labels_wells <- sprintf("%s: %g", "Qw= ", wells$Q_m3s) #%>% lapply(htmltools::HTML)

# 下水道施設
sewage_facility <- read.csv("data/static-data/sewage_facility_H24(H22).csv", encoding = "UTF-8")
sf_name <- sewage_facility[,3]
sf_population <- sewage_facility[,4]
labels_sewage_facility <- sprintf("%s: %g", sf_name, sf_population) %>% lapply(htmltools::HTML)
# 水道取水口
sluice_gate <- readRDS("data/static-data/sluice_gate.rds")

#-----------------------------------
# PRTR Data:
#-----------------------------------

# individual stations (1 row per station), all  -> Update prtr_station_all with New PRTR data from Otani-sensei (OLD,NEW,ALL)
## OLD
prtr_station_old <- fread("data/prtr/prtr_station_new.csv", encoding = "UTF-8") # 16274
prtr_station_old$address3[is.na(prtr_station_old$address3)] <- "" # make blank for when pasting address (some components of address are NA)
prtr_station1 <- prtr_station_old %>% filter(!is.na(linkID)) # just those that have an linkID, 2423

## NEW
prtr_station_new <- st_read("data/prtr/PRTR_2019_select/PRTR_2019_select.shp") # %>% st_transform(crs=4326) # 1357
df_s <- prtr_station_new %>% st_drop_geometry %>% bind_cols(prtr_station_new %>% st_coordinates) %>% dplyr::select(27,2,1,13,14,16:21,Y,X,3)
df_s$X2 <- df_s$X.U.6CB3..U.5DDD..U.540D.; df_s$X3 <- 1; names(df_s) <- names(prtr_station_old)
prtr_station_new <- df_s # 1357
prtr_station_new$address3[is.na(prtr_station_new$address3)] <- "" # make blank for when pasting address (some components of address are NA)
prtr_station2 <- prtr_station_new %>% filter(!is.na(linkID)) # just those that have an linkID, 1357

## ALL
prtr_station_all <- rbind(prtr_station_old,prtr_station_new) # 17631
#moved up, delete if works########### prtr_station2 <- prtr_station_new %>% filter(!is.na(linkID)) # just those that have an linkID, 1357

# PRTR and rivers -> Update prtr_data with New PRTR data from Otani-sensei (OLD, NEW, ALL)
prtr_data_old <- fread("data/prtr/prtr_data.csv", encoding = "UTF-8") # all linkID rivers per station, 108816
prtr_data_new <- df_s # 1357
prtr_data_all <- rbind(prtr_data_old,prtr_data_new)

# Rivers linked to PRTR stations -> Update prtr_river_all with New PRTR data from Otani-sensei (OLD, NEW, ALL)
prtr_river_old <- st_read("data/prtr/rivers, lakes/rvr_tmdu.shp")   # %>% st_transform(crs=4326) # just rivers in TMDU region, 980 rivers
prtr_river_new <-   st_read("data/prtr/W05_select/W05_select_p.shp")  # %>% st_transform(crs=4326) # new rivers (with, without linkID) 51121
prtr_river_all <- rbind(prtr_river_old %>% dplyr::select(LinkID), prtr_river_new %>% rename(LinkID=W05_004) %>% dplyr::select(LinkID)) # 31823 x52101
prtr_river_new$W05_004 %<>% paste(prtr_river_new$W05_010,sep="-")
prtr_river1 <- prtr_river_old[prtr_river_old$LinkID %in% as.character(prtr_station1$upper),] # %>% st_transform(crs=4326) # just rivers that have links ,
prtr_river2 <- prtr_river_new[(gsub("-.*","",prtr_river_new$W05_004)) %in% as.character(prtr_station2$upper),] # %>% st_transform(crs=4326) # just rivers that have links ,

# Lakes and IDs linking lakes to rivers (only keep BIWAKO)
prtr_lake <- st_read("data/prtr/rivers, lakes/lake.shp")  # %>% st_transform(crs=4326)  %>% filter(LinkID==251002) 
lake_river_id <- fread("data/prtr/rivers, lakes/lake_river_id.csv", encoding = "UTF-8")[,-2]  %>% filter(lakeID==251002)

# Simulation layers for Air and River 
Air <- fread("data/riversim/atmos_210224.csv", encoding = "UTF-8", col.names = c("x", "AEGL1", "AEGL2", "AEGL3"))
layers <- st_layers("data/riversim/⑤水域kmlデータ.kml")
for (i in 1:4) assign(paste0("layer",i), st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[i])) 
# layer1 <- st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[1])
# layer2 <- st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[2])
# layer3 <- st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[3])
# layer4 <- st_read("data/riversim/⑤水域kmlデータ.kml", layer = layers$name[4])

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

# Default basemap -> change to Open Street Map, not Google Maps
basemap <- MAP_TILES_URLS$OPEN_STREET_MAP
til <- MAP_TILES_DESCRIPTION$OPEN_STREET_MAP

# HOTLINE SETTINGS ---------------------

radiusInfluence  <- NULL
bufferDistance <- NULL
hotlineOutlineWidth <- NULL
hotlineWeight <- NULL
hotlineSmooth <- NULL
hotlineOutlineColor <- NULL
rainbowPallet <- NULL

# GROUNDWATER SETTINGS ---------------------
gridTransparency <- NULL
gridColor <- NULL
gridWeight <- NULL
arrowOpacity <- NULL
arrowColor <- NULL
arrowWeight <- NULL
arrowLength <- NULL
sourceLocationColor <- NULL
sourceLocationOpacity <- NULL
sourceLocationWeight <- NULL

contourSmoothAuto <- NULL
contourSmooth <- NULL
contourOpacity <- NULL