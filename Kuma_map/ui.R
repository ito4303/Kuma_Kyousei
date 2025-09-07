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
      # label for past incidents
      h3("過去の出没情報"),
      p("◎で表示しています"),
      # checkbox group for filtering by year
      checkboxGroupInput(
        inputId = "checkbox_year", 
        label = "出没年", 
        choices = as.character(2019:2024),
        selected = as.character(2019:2024),
        width = "95%"
      ),

      # label for prediction
      h3("2025年の出没予測確率"),
      #p("タイルで表示しています"),
      # slider for opacity of prediction
      sliderInput(
        inputId = "slider_opacity",
        label = "予測確率タイルの不透明度",
        min = 0,
        max = 1,
        value = 0.7,
        step = 0.1,
        width = "95%"
      )
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
      width = 8
    )
  ),
  padding = 10
)
