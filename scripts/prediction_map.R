#
# map viewing predicted probability of bear insident occurrence
#

library(tidyverse)
library(sf)
library(leaflet)

# load data
kuma_data <- file.path("data", "kuma_data.parquet") |>
  nanoparquet::read_parquet() |>
  # Select relevant columns and rename them for clarity
  dplyr::mutate(
    year = `出没年`,
    longitude = `経度`,
    latitude = `緯度`,
    type = case_match(
      `目撃痕跡種別`,
      "目撃" ~ "目撃",
      "痕跡" ~ "痕跡",
      .default = "人身被害・その他"),
    comment = stringr::str_c(
      year(`出没日`), "年", month(`出没日`), "月", day(`出没日`), "日 ",
      htmltools::htmlEscape(`時刻`),
      "<br />",
      if_else(`森林からの出没`, "【森林からの出没】", ""),
      if_else(`河川からの出没`, "【河川からの出没】", ""),
      if_else(`誘引物が原因の出没`, "【誘引物が原因の出没】", ""),
      if_else(`繁殖・分散行動による出没`, "【繁殖・分散行動による出没】", ""),
      if_else(`大量出没年に特有の出没`, "【大量出没年に特有の出没】", ""),
      "<br />",
      htmltools::htmlEscape(`備考`)
    ),
    .keep = "none"
  )

prob_data <- file.path("data", "kuma_prediction.geojson") |>
  sf::st_read() |>
  dplyr::mutate(prob = est * 100) |>   # percent
  sf::st_transform(crs = "WGS84")

# map
leaflet() |>
  addTiles() |>
  setView(136.6, 36.8, zoom = 9) |>
  addPolygons(data = prob_data,
              fillColor = colorNumeric("YlOrRd", domain = c(0, 100))(prob_data$prob),
              fillOpacity = 0.6,
              stroke = FALSE,
              popup = sprintf("%2.1f%%", prob_data$prob)) |>
  addLegend(position = "topright",
            pal = colorNumeric("YlOrRd", domain = c(0, 100)),
            values = c(0, 100),
            title = "確率(%)",
            opacity = 1) |>
  addCircleMarkers(data = kuma_data,
                   color = ~case_when(
                     type == "目撃" ~ pal_colors[6],
                     type == "痕跡" ~ pal_colors[7],
                     .default = pal_colors[1]),
                   opacity = 0.7,
                   popup = ~comment,
                   clusterOptions = TRUE) |>
  addLegend(data = kuma_data,
            position = "bottomright",
            colors = pal_colors[c(6, 7, 1)],
            title = "出没タイプ",
            labels = c("目撃", "痕跡", "人身被害・その他"))

