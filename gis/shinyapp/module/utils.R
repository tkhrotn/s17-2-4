drawFunction <- function(map, data, param) {
  switch (param$type,
          "AwesomeMarkers" = {
            addAwesomeMarkers(map,
                              data = data, lng = ~lng, lat = ~lat,
                              label = lapply(data$label, htmltools::HTML), popup = data$popup, layerId = data$layerId,
                              icon = param$icon, group = param$group)
          },
          "CircleMarkers" = {
            addCircleMarkers(map,
                       data = data, lng = ~lng, lat = ~lat,
                       label = data$label, popup = data$popup,
                       group = param$group, radius = param$radius,
                       color = param$color, stroke = param$stroke,
                       weight = param$weight, opacity = param$opacity,
                       fill = param$fill, fillColor = param$fillColor,
                       fillOpacity = param$fillOpacity)
          },
          map
          )
}


# This tells htmlwidgets about our plugin name, version, and
# where to find the script. (There's also a stylesheet argument
# if the plugin comes with CSS files.)
vectorGridPlugin <- htmlDependency("Leaflet.VectorGrid", "1.3.0",
                             src = c(href = "./js/"),
                             script = "Leaflet.VectorGrid.bundled.js"
)

# A function that takes a plugin htmlDependency object and adds
# it to the map. This ensures that however or whenever the map
# gets rendered, the plugin will be loaded into the browser.
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
