######################################################################
#
# Project Settings
#
######################################################################

library("rjson")

# HELPER FUNCTIONS ---------------------

# Set hotline configure
setHotlineConfig <- function () {
  
  # Read data from settings.json file
  jsonFile <- 'data/settings.json'
  if (file.exists(jsonFile)) {
    projectSettings <<- fromJSON(file = jsonFile)
  }
  
  hotlineOutlineWidth <<- projectSettings$hotlineOutlineWidth
  hotlineWeight <<- projectSettings$hotlineWeight
  hotlineSmooth <<- projectSettings$hotlineSmooth
  hotlineOutlineColor <<- projectSettings$hotlineOutlineColor
  rainbowPallet <<- projectSettings$rainbowPallet
  bufferDistance <<- projectSettings$bufferDistance
  radiusInfluence <<- projectSettings$radiusInfluence

  gridTransparency <<- projectSettings$gridTransparency
  gridColor <<- projectSettings$gridColor
  gridWeight <<- projectSettings$gridWeight

  arrowOpacity <<- projectSettings$arrowOpacity
  arrowColor <<- projectSettings$arrowColor
  arrowWeight <<- projectSettings$arrowWeight
  arrowLength <<- projectSettings$arrowLength
  sourceLocationColor <<- projectSettings$sourceLocationColor
  sourceLocationOpacity <<- projectSettings$sourceLocationOpacity
  sourceLocationRadius <<- projectSettings$sourceLocationRadius

  contourSmoothAuto <<- projectSettings$contourSmoothAuto
  contourSmooth <<- projectSettings$contourSmooth
  contourOpacity <<- projectSettings$contourOpacity
  
  hotlineOutlineWidth <<- ifelse(is.null(hotlineOutlineWidth), DEFAULT_OUTLINE_WIDTH, as.numeric(hotlineOutlineWidth))
  hotlineWeight <<- ifelse(is.null(hotlineWeight), DEFAULT_WEIGHT, as.numeric(hotlineWeight))
  hotlineSmooth <<- ifelse(is.null(hotlineSmooth), DEFAULT_SMOOTH, as.numeric(hotlineSmooth))
  hotlineOutlineColor <<- ifelse(is.null(hotlineOutlineColor), DEFAULT_OUTLINE_COLOR, as.character(hotlineOutlineColor))
  bufferDistance <<- ifelse(is.null(bufferDistance), 1500, as.numeric(bufferDistance))
  radiusInfluence <<- ifelse(is.na(as.numeric(radiusInfluence)) || is.null(radiusInfluence), 1000, as.numeric(radiusInfluence))
  
  gridTransparency <<- ifelse(is.null(gridTransparency), F, gridTransparency)
  gridColor <<- ifelse(is.null(gridColor), '#1B5EDB', gridColor)
  gridWeight <<- ifelse(is.null(gridWeight), 2, gridWeight)

  arrowOpacity <<- ifelse(is.null(arrowOpacity), 0.8, arrowOpacity)
  arrowColor <<- ifelse(is.null(arrowColor), '#fc0f03', arrowColor)
  arrowWeight <<- ifelse(is.null(arrowWeight), 4, arrowWeight)
  arrowLength <<- ifelse(is.null(arrowLength), 0.2, arrowLength)
  sourceLocationColor <<- ifelse(is.null(sourceLocationColor), '#fc0f03', sourceLocationColor)
  sourceLocationOpacity <<- ifelse(is.null(sourceLocationOpacity), 0.8, sourceLocationOpacity)
  sourceLocationRadius <<- ifelse(is.null(sourceLocationRadius), 20, sourceLocationRadius)

  contourSmoothAuto <<- ifelse(is.null(contourSmoothAuto), T, contourSmoothAuto)
  contourSmooth <<- ifelse(is.null(contourSmooth), 5, contourSmooth)
  contourOpacity <<- ifelse(is.null(contourOpacity), 0.9, contourOpacity)
  
  if (is.null(rainbowPallet)) {
    rainbowPallet <<- DEFAULT_PALLET_COLOR
  } else {
    rainbowPallet <<- as.vector(unlist(rainbowPallet, use.names = FALSE))
  }
}

# Update five input colors
updateColor <- function (colors) {
  updateColourInput(session, 'color1', '', colors[1])
  updateColourInput(session, 'color2', '', colors[2])
  updateColourInput(session, 'color3', '', colors[3])
  updateColourInput(session, 'color4', '', colors[4])
  updateColourInput(session, 'color5', '', colors[5])
}

# Update input hotline settings
updateInputValue <- function () {
  updateNumericInput(session, 'srcROI', value=radiusInfluence)
  updateNumericInput(session, 'bufferDistance', value=bufferDistance)
  updateNumericInput(session, 'hotlineOutlineWidth', value=hotlineOutlineWidth)
  updateNumericInput(session, 'hotlineWeight', value=hotlineWeight)
  updateNumericInput(session, 'hotlineSmooth', value=hotlineSmooth)
  updateTextInput(session, 'hotlineOutlineColor', value=hotlineOutlineColor)
  updateTextInput(session, 'rainbowPalletSelected', value=rainbowPallet)
  updateColor(rainbowPallet)

  updateCheckboxInput(session, 'gridTransparency', value=gridTransparency)  
  updateColourInput(session, 'gridColor', value=gridColor)  
  updateNumericInput(session, 'gridWeight', value=gridWeight)  
  updateCheckboxInput(session, 'contourSmoothAuto', value=contourSmoothAuto)  
  updateNumericInput(session, 'contourSmooth', value=contourSmooth)  
  updateNumericInput(session, 'contourOpacity', value=contourOpacity) 

  updateNumericInput(session, 'arrowOpacity', value=arrowOpacity)  
  updateColourInput(session, 'arrowColor', value=arrowColor)  
  updateNumericInput(session, 'arrowWeight', value=arrowWeight)  
  updateNumericInput(session, 'arrowLength', value=arrowLength)  
  updateColourInput(session, 'sourceLocationColor', value=sourceLocationColor)  
  updateNumericInput(session, 'sourceLocationOpacity', value=sourceLocationOpacity)  
  updateNumericInput(session, 'sourceLocationRadius', value=sourceLocationRadius)  
}

# Save confiugre to settings.json file
saveSettings <- function () {

  color1 <- input$color1
  color2 <- input$color2
  color3 <- input$color3
  color4 <- input$color4
  color5 <- input$color5
  
  outputSettings <- list()
  outputSettings$hotlineOutlineWidth <- as.numeric(input$hotlineOutlineWidth)
  outputSettings$hotlineWeight <- as.numeric(input$hotlineWeight)
  outputSettings$hotlineSmooth <- as.numeric(input$hotlineSmooth)
  outputSettings$hotlineOutlineColor <- as.character(input$hotlineOutlineColor)
  outputSettings$rainbowPallet <- c(color1, color2, color3, color4, color5)
  outputSettings$bufferDistance <- as.numeric(input$bufferDistance)
  outputSettings$radiusInfluence <- as.numeric(input$radiusInfluence)

  outputSettings$gridTransparency <- input$gridTransparency
  outputSettings$gridColor <- input$gridColor
  outputSettings$gridWeight <- as.numeric(input$gridWeight)
  outputSettings$contourSmoothAuto <- input$contourSmoothAuto
  outputSettings$contourSmooth <- as.numeric(input$contourSmooth)
  outputSettings$contourOpacity <- as.numeric(input$contourOpacity)

  outputSettings$arrowOpacity <- as.numeric(input$arrowOpacity)
  outputSettings$arrowColor <- input$arrowColor
  outputSettings$arrowWeight <- as.numeric(input$arrowWeight)
  outputSettings$arrowLength <- as.numeric(input$arrowLength)
  outputSettings$sourceLocationColor <- input$sourceLocationColor
  outputSettings$sourceLocationOpacity <- as.numeric(input$sourceLocationOpacity)
  outputSettings$sourceLocationRadius <- as.numeric(input$sourceLocationRadius)

  projectSettings <- toJSON(outputSettings)  
  write(projectSettings, file="data/settings.json")
  
  if (input$tabs == 'ctat') {
    setHotlineConfig()
  }
  if (input$tabs == 'groundwater') {
    updateGroundwaterSettings()
  }
  updateInputValue()
  if (input$tabs == 'ctat') {
    updateHotlineSettings()
    updateHotline()
  }
}


# GLOBAL VAIRABLES ---------------------

projectSettings <- NULL


# INITIALIZE UI ---------------------

setHotlineConfig()
updateInputValue()

# REACTIVE ---------------------

rainbowPalletSelected <- reactive(as.vector(gsub(".+\\s+", "", input$rainbowPalletSelected)))


# OBSERVE ---------------------

observe({
    rainbowPalletSelected <- rainbowPalletSelected()
    updateColor(rainbowPalletSelected)
})


# OBSERVE EVENT ---------------------

observeEvent(input$saveHotlineSettings, saveSettings())
observeEvent(input$saveDistance, saveSettings())
observeEvent(input$saveGroundwaterSettings, saveSettings())


