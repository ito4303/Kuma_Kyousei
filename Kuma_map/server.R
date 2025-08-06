#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)

function(input, output, session) {
  output$map <- renderLeaflet({
    kuma_data |>
    leaflet() |>
      addTiles() |>
      setView(136.6, 36.8, zoom = 9) |>
    addLegend(
      position = "bottomright",
      colors = pal_colors[c(6, 7, 1)],
      labels = c("目撃", "痕跡", "人身被害・その他"))
  })
  
  # checkboxGroupInput is used to filter the data
  # and update the map based on selected years
  observeEvent(input$checkbox_group, {
    leafletProxy(
      mapId = "map",
      data = kuma_data |>
        dplyr::filter(as.character(year) %in% input$checkbox_group)
    ) |>
      clearMarkerClusters() |>
      clearMarkers() |>
      addCircleMarkers(
        color = ~case_when(
          type == "目撃" ~ pal_colors[6],
          type == "痕跡" ~ pal_colors[7],
          .default = pal_colors[1]),
        opacity = 0.7,
        popup = ~comment,
        clusterOptions = TRUE
      )
  },
  ignoreNULL = FALSE)
}
