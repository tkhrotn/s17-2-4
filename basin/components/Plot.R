######################################################################
#
# Boxplot and Timeseries of data location
#
######################################################################

output$ts_Q <- renderPlotly({
  data <- data()
  if (is.null(data)) return(NULL)
  dataxts <- NULL
  obs <- unique(data$NAME)
  for(l in 1:length(obs)){
    datastation <- data[data$NAME == obs[l],]
    dayaxis <- as.Date(datastation$YMD)
    dd <- xts(datastation[datastation$ITEM == "河川流量",5], dayaxis, frequency = 365.25)
    dataxts <- cbind(dataxts, dd)
  }
  colnames(dataxts) <- as.character(obs[1:ncol(dataxts)])
  dataxts <- as.data.frame(dataxts) #important, "金田町","広芝町","江の木町","小曽根三丁目","豊南町東三丁目","稲津町三丁目"
  plot_ly(x = ~dayaxis, y = ~dataxts[,1], type = "scatter", mode = "lines", name = colnames(dataxts)[1], text = paste(colnames(dataxts)[1], dayaxis, ":", 1)) %>%
    add_trace(x = ~dayaxis, y = dataxts[,2], type = "scatter", mode = "lines",  name = colnames(dataxts)[2], text = paste(colnames(dataxts)[2], dayaxis, ":",2)) %>%
    add_trace(x = ~dayaxis, y = dataxts[,3], type = "scatter", mode = "lines",  name = colnames(dataxts)[3], text = paste(colnames(dataxts)[3], dayaxis, ":",2)) %>%
    add_trace(x = ~dayaxis, y = dataxts[,4], type = "scatter", mode = "lines",  name = colnames(dataxts)[4], text = paste(colnames(dataxts)[4], dayaxis, ":",2)) %>%
    add_trace(x = ~dayaxis, y = dataxts[,5], type = "scatter", mode = "lines",  name = colnames(dataxts)[5], text = paste(colnames(dataxts)[5], dayaxis, ":",2)) %>%
    add_trace(x = ~dayaxis, y = dataxts[,6], type = "scatter", mode = "lines",  name = colnames(dataxts)[6], text = paste(colnames(dataxts)[6], dayaxis, ":",2)) %>%
    add_trace(x = ~dayaxis, y = dataxts[,7], type = "scatter", mode = "lines",  name = colnames(dataxts)[7], text = paste(colnames(dataxts)[7], dayaxis, ":",2)) %>%
    layout(
      title = "河川流量",
      xaxis = list(showgrid = T, title = "年"),
      yaxis = list(showgrid = F, title = "排出量(m3/s)", side="left"),
      yaxis2= list(title = "雨量(mm)", overlaying = "y",  side="right", ticks="outside"),
      yaxis = list(type = "log"),
      showlegend = TRUE
    ) %>%
    config(locale = 'ja', toImageButtonOptions = list(format="png", filename="TMDU_TIMESERIES"))

})

output$boxp_Q <- renderPlotly({
  data <- data()
  if (is.null(data)) return(NULL)
  dataxts <- NULL
  obs <- unique(data$NAME)
  for (l in 1:length(obs[1:7])){
    datastation <- data[data$NAME == obs[l],]
    dayaxis <- as.Date(datastation$YMD)
    dd <- datastation[datastation$ITEM == "河川流量", datastation[,5]]
    dataxts <- cbind(dataxts, dd)
  }
  colnames(dataxts) <- as.character(obs[1:ncol(dataxts)])
  dataxts <- as.data.frame(dataxts) #important, Akutagawa,Yoshidabashi,Takahama,Oobe,Kamitodoromi,Hirakata,Tokura
  
  plot_ly(alpha = 0.8, type ="box") %>%
    add_trace(x = dataxts[,1], name=colnames(dataxts)[1]) %>%
    add_trace(x = dataxts[,2], name=colnames(dataxts)[2]) %>%
    add_trace(x = dataxts[,3], name=colnames(dataxts)[3]) %>%
    add_trace(x = dataxts[,4], name=colnames(dataxts)[4]) %>%
    add_trace(x = dataxts[,5], name=colnames(dataxts)[5]) %>%
    add_trace(x = dataxts[,6], name=colnames(dataxts)[6]) %>%
    add_trace(x = dataxts[,7], name=colnames(dataxts)[7]) %>%
    layout(title = "排出量", xaxis = list(title = "排出量(m3/s)")) %>%
    config(locale = 'ja', toImageButtonOptions = list(format="png", filename="TMDU_BOXPLOT"))

})
