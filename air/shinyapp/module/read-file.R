library(openxlsx)
library(lubridate)

readCSV <- function(filepath) {
  data <- read.csv(filepath, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  return(data)
}

readXLSX <- function(filepath) {
  read.xlsx(filepath)
}

readFile <- function(filepath) {
  ext <- gsub(gsub("\\..+$", "", basename(filepath)), "", basename(filepath))
  
  data <- switch(ext,
    ".csv" = readCSV(filepath),
    ".xlsx" = readXLSX(filepath)
  )
  
  return(convert2Dataset(data))
}

convert2Dataset <- function(data) {
  NAME_COL <- 1
  LNG_COL <- 2
  LAT_COL <- 3
  UNIT_COL <- 4
  
  dataset <- data.frame()
  for (i in 5:ncol(data)) {
    d <- data.frame(group_name=colnames(data)[i],
                    location_name=data[,NAME_COL],
                    longitude=data[,LNG_COL],
                    latitude=data[,LAT_COL],
                    value=data[,i],
                    remarks=data[,UNIT_COL]
                    )

    dataset <- rbind(dataset, d)
  }
  
  return(dataset)
}
