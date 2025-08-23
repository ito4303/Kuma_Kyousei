#
# 「クマ×共生ハッカソン」のデータ前処理
#

# パッケージを読み込みます。
library(data.table)
library(tidyverse)
library(nanoparquet)
library(jpmesh)

# データが保存されているディレクトリとファイル名を指定します。
data_dir <- file.path("data", "original")
data_files <-
  c("bear_incidents_forest_r1-r6.csv",
    "bear_incidents_river_r1-r6.csv",
    "bear_incidents_attractants_r1-r6.csv",
    "bear_incidents_young_bears_r1-r6.csv",
    "bear_incidents_mass_outbreak_r2.csv")
  # "bear_sightings_r7.csv" は今回は使用しません。

# データを読み込む関数を定義します。
read_fun <- function(file_path) {
  col_types <- str_c(str_dup("c", 13), str_dup("d", 3))

  data.table::fread(file_path, header = TRUE) |>
    dplyr::mutate(
      `出没年` = as.numeric(str_sub(`出没年`, 2)) + 2018,
      `出没日` = case_when(
        # 無記入の場合、欠損値に
        `出没日` == "" ~ as.Date(NA),
        # Excelのシリアル値の場合
        str_detect(`出没日`, "^4[0-9]{4}$") ~
          as.Date(as.numeric(`出没日`, tz = "Asia/Tokyo"),
                  origin = "1899-12-30"),
        # "R1..."という形式の場合
        str_detect(`出没日`, "^R[1-9]") ~
          str_replace(`出没日`, "^R[1-9]",
                      as.character(`出没年`)) |>
          ymd(tz = "Asia/Tokyo"),
        # "2019...""という形式の場合
        str_detect(`出没日`, "^2[0-9]{3}") ~
          ymd(`出没日`, tz = "Asia/Tokyo"),
        # その他は欠損値に
        TRUE ~ as.Date(NA)),
      # 頭数を整数に変換, エラーになるものは個別に対処
      `頭数` = if_else(!is.na(as.integer(`頭数`)),
                       # 変換が成功するとき
                       as.integer(`頭数`),
                       # 変換が失敗するとき
                       case_match(
                         `頭数`,
                         c("１", "1頭", "１頭", "不明(1か2)",
                           "体長1ｍ程度と推定される成獣",
                           "成獣１頭") ~ 1,
                         c("2〜3頭", "2頭") ~ 2,
                         c("3頭") ~ 3,
                         c("4〜5頭", "4頭") ~ 4,
                        .default = as.integer(NA))),
      # IDが年をまたいでも重複しないように、年 + ID とします。
      ID = str_c(`出没年`, ID, sep = "-"),
      # 500mメッシュコードは、jpmesh::coords_to_mesh()を使用して、
      # 経度・緯度から計算し直します。
      `500mメッシュコード` = coords_to_mesh(`経度`, `緯度`,
                                     to_mesh_size = 0.5) |>
                             as.character()
    ) |>
    as_tibble()
}

# データを読み込みます。
# case_match()ではいったん各要素を評価するので警告がでますが
# これは問題ではありません。
data_t <- purrr::map(
  data_files,
  \(f) read_fun(file.path(data_dir, f))
)

# データを結合します。
data <- data_t |>
  dplyr::bind_rows() |>
  dplyr::select(1:15) |>
  dplyr::distinct(ID, .keep_all = TRUE) |>
  dplyr::left_join(dplyr::select(data_t[[1]], c(2, 16)),
                   by = "ID", na_matches = "never") |>
  dplyr::left_join(dplyr::select(data_t[[2]], c(2, 16)),
                   by = "ID", na_matches = "never") |>
  dplyr::left_join(dplyr::select(data_t[[3]], c(2, 16)),
                   by = "ID", na_matches = "never") |>
  dplyr::left_join(dplyr::select(data_t[[4]], c(2, 16)),
                   by = "ID", na_matches = "never") |>
  dplyr::left_join(dplyr::select(data_t[[5]], c(2, 16)),
                   by = "ID", na_matches = "never") |>
  dplyr::mutate_at(16:20, \(x) !is.na(x))

# 出没日がdatetime型になっているので、Date型に直します。
data <- data |>
  dplyr::mutate(`出没日` = as.Date(`出没日`, tz = "Asia/Tokyo"))


# CSVとParquet形式で保存します。
data_dir <- file.path("data")
write_csv(data, file = file.path(data_dir, "kuma_data.csv"))
write_parquet(data, file = file.path(data_dir, "kuma_data.parquet"))
