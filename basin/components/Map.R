# Contamination Transport & Arrival Time
eval(parse("components/Map/CTAT.R", encoding="UTF-8"))

# Groundwater Transport
eval(parse("components/Map/Groundwater.R", encoding="UTF-8"))


map_bounds <- NULL
map_zoom <- NULL

# OBSERVE ---------------------

# Clear previous Tab
observe({
    currentTab <- input$tabs

    isolate({
        if (currentTab == 'ctat') {
            session$sendCustomMessage('onRemoveContour', '')
            # Remove Legend of GW
            leafletProxy(mapId = "map") %>% removeControl(layerId = "legend_contour")
        } else if (currentTab == 'groundwater') {
            buildMap(refresh=TRUE)
            LMap <- leafletProxy(mapId = "map")
            if (!is.null(map_bounds)) {
                LMap %>% fitBounds(lng1 = map_bounds$east, lat1 = map_bounds$north, lng2 = map_bounds$west, lat2 = map_bounds$south )
                map_bounds <<- NULL
            }
        }
    })
})

observe({
    input$map_bounds
    currentTab <- input$tabs

    isolate({
        if (currentTab == 'ctat') {
            map_bounds <<- input$map_bounds
            map_zoom <<- input$map_zoom
        }
    })
})
