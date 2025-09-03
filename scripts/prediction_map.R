#
# map viewing predicted probability of bear insident occurrence
#

library(tidyverse)
library(sf)
library(leaflet)

# load data
prob_data <- sf::st_read("data/kuma_prediction.geojson") |>
  dplyr::mutate(prob = est * 100) |>   # percent
  sf::st_transform(crs = "WGS84")

# map
leaflet() |>
  addTiles() |>
  setView(lng = 136.7, lat = 36.6, zoom = 11) |>
  addPolygons(data = data,
              fillColor = colorNumeric("YlOrRd", domain = c(0, 100))(prob_data$prob),
              fillOpacity = 0.6,
              stroke = FALSE,
              popup = sprintf("%2.1f%%", prob_data$prob)) |>
  addLegend(position = "topright",
            pal = colorNumeric("YlOrRd", domain = c(0, 100)),
            values = c(0, 100),
            title = "確率(%)",
            opacity = 1)
