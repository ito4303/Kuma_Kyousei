#
# This is the global settings of a Shiny web application.
#

library(tidyverse)

# Load the bear data from a parquet file
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

# Load the prediction data
prob_data <- file.path("data", "kuma_prediction.geojson") |>
  sf::st_read() |>
  dplyr::mutate(prob = pred * 100) |>   # percent
  sf::st_transform(crs = "WGS84")

# palette colors
pal_colors <- palette.colors(n = 8, palette = "Okabe-Ito")
