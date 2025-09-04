#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)

fillPage(

  # App title
  titlePanel("石川県クマ出没マップ"),
  
  sidebarLayout(
    sidebarPanel(
      # checkbox group for filtering by year
      checkboxGroupInput(
        inputId = "checkbox_year", 
        label = "出没年", 
        choices = as.character(2019:2024),
        selected = as.character(2019:2024),
        width = "95%"
      ),
      checkboxInput(
        inputId = "checkbox_prediction",
        label = "出没予測確率の表示",
        value = TRUE,
        width = "95%"
      ),
      width = 2
    ),
    
    # Show the map
    mainPanel(
      # c.f. https://blog.atusy.net/2019/08/01/shiny-plot-height/
      div (
        leafletOutput(
          outputId = "map",
          width = "95%",
          height = "100%"
        ),
        style = "height: calc(100vh  - 100px)"
      ),
      width = 10
    )
  ),
  padding = 10
)
