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
    leaflet() |>
      addTiles() |>
      setView(136.6, 36.8, zoom = 9) |>
      addLegend(
        position = "topright",
        colors = pal_colors[c(6, 7, 1)],
        title = "出没タイプ",
        labels = c("目撃", "痕跡", "人身被害・その他"),
        data = kuma_data
      ) |>
      addLegend(
        position = "bottomright",
        pal = colorNumeric("YlOrRd", domain = c(0, 100)),
        values = c(0, 100),
        title = "出没確率(%)",
        opacity = 1,
        data = prob_data
      )
  })

  # checkboxGroupInput is used to filter the data
  # and update the map based on selected years
  observeEvent(input$checkbox_year, {
    proxy <- leafletProxy(
      mapId = "map",
      data = kuma_data |>
        dplyr::filter(as.character(year) %in% input$checkbox_year)
    )
    proxy |>
      clearMarkerClusters() |>
      clearMarkers() |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~case_when(
          type == "目撃" ~ pal_colors[6],
          type == "痕跡" ~ pal_colors[7],
          .default = pal_colors[1]),
        opacity = 0.7,
        popup = ~comment,
        clusterOptions = TRUE
      )},
  ignoreNULL = FALSE)
  
  # checkboxInput is used to toggle the prediction layer
  observeEvent(input$checkbox_prediction, {
    proxy <- leafletProxy(
      mapId = "map",
      data = prob_data
    )
    if (input$checkbox_prediction) {
      proxy |>
        addPolygons(
          fillColor = colorNumeric("YlOrRd", domain = c(0, 100))(prob_data$prob),
          fillOpacity = 0.6,
          stroke = FALSE,
          popup = sprintf("%2.1f%%", prob_data$prob)
        )
    } else {
      proxy |>
        clearShapes()
    }
  })
}
