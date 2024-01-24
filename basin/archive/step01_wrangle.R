# Step 1 - Data Wrangling

# Overall process;
# - read in data PRTR and rivers, filter just Yodogawa, make one data.frame
# - select station
# - get info of station to show on card
# - filter by ID and display rivers with those IDs
# - distinguish between kousui and gesui 
# - update UI
# - create new functions embedded in Map.R
# how to display all stations and rivers, but highlight specific station, rivers on top? 
# https://rstudio.github.io/leaflet/morefeatures.html

# packages
pacman::p_load(magrittr, leaflet, htmltools, sp, sf, maps, rgdal, jsonlite)
Sys.setlocale("LC_ALL", "English_United States.932")

# read in PRTR data
wd <- paste0(getwd(),"/data/prtr/")
prtr <- read.csv(paste0(wd,"prtr.csv"),encoding = "UTF-8")
# dim(prtr)
# prtr %>% View

# read in GIS data
# wd <- paste0(getwd(),"/data/gis/") 
# lake <- readOGR(paste0(wd,"lake.shp")) # just lake in Yodogawa region
river <- readOGR(paste0(wd,"rvr_zenkoku.shp")) # just rivers of Yodogawa region

# LinkID of river
river@data %>% head

# LinkID of river in one PRTR station
prtr %>% subset(整理番号==整理番号[1] & 公水フラグ!="") %>% dplyr::select(target_BSN)
prtr %>% subset(整理番号==整理番号[1] & 下水道フラ!="") %>% dplyr::select(target_BSN)
prtr %>% subset(整理番号==整理番号[1]) %>% dplyr::select(upper_BSNL)

# fix public, sewage
# 1=public, 2=sewage,3=both
# prtr_data <- read.csv(paste0(wd,"prtr_data.csv"),encoding = "UTF-8")
prtr_data <- prtr
prtr_data$public <- 3 # both
prtr_data$public[prtr$公水フラグ==""] <- 2 # if public is "", it is sewage
prtr_data$public[prtr$下水道フラ==""] <- 1 # if sewage is "", it is public
names(prtr_data) <- c("linkID","upper","ID","public","company.name1","company.name2","address1","address2","address3","nb","company.type","code","lat","lng")
# View(prtr_data)
# write.csv(prtr_data, file=paste0(wd,"prtr_data2.csv"), fileEncoding = "UTF-8")
fwrite(prtr_data, file=paste0(wd,"prtr_data.csv"))

# station data (1 row per station)
out <- prtr_data[1,]
stn <- prtr_data$ID %>% unique
for (i in 1:length(stn)) out[i,] <- prtr_data[which(prtr_data$ID==stn[i]),][1,]
fwrite(out, file=paste0(wd,"prtr_station.csv"))

