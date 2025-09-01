#
# map viewing predicted probability of bear insident occurrence
#

library(tidyverse)
library(sf)
library(leaflet)

# load data
data <- sf::st_read("data/kuma_prediction.geojson") |>
  sf::st_transform(crs = "WGS84")

# map
leaflet() |>
  addTiles() |>
  setView(lng = 136.7, lat = 37.0, zoom = 10) |>
  addPolygons(data = data,
              fillColor = colorNumeric("YlOrRd", domain = c(0, 1))(data$est),
              fillOpacity = 0.7,
              stroke = FALSE)
