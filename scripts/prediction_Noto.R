#
# 石川県能登地方クマ出没予測マップの作成
#

# パッケージの読み込み
library(tidyverse)
library(sf)
library(nanoparquet)
library(sdmTMB)

# 擬似乱数のシード
set.seed(1234)

# データ配置ディレクトリ
data_dir <- "data"

# クマ出没データのパス
data_file <- file.path(data_dir, "kuma_data.parquet")

# 土地利用データのパス
landuse_files <- 
  c("L03-a-21_5436.geojson",
    "L03-a-21_5536.geojson",
    "L03-a-21_5537.geojson",
    "L03-a-21_5636.geojson",
    "L03-a-21_5637.geojson") |>
  purrr::map_chr(\(f) file.path(data_dir, "original", f))

# 石川県行政区域データのパス
ishikawa_pref_file <- file.path(data_dir, "original",
                                "N03-20250101_17.geojson")

# クマ出没データの読み込み
kuma_data <- read_parquet(data_file) |>
  # 500mメッシュコードを3次(1km)メッシュコードに変換
  dplyr::mutate(mesh_code = str_sub(`500mメッシュコード`,1, 8))

# 土地利用データの読み込み
landuse <- landuse_files %>%
  purrr::map(\(f) {
    sf::st_read(f, quiet = TRUE)
  }) |>
  dplyr::bind_rows() |>
  # m^2単位をkm^2単位にする
  dplyr::mutate_at(2:14, `/`, 10^6) |>
  # 3次メッシュコードを文字型にする
  dplyr::mutate(mesh_code = as.character(`メッシュ`))

# 石川県の行政区域データの読み込み
noto <- sf::st_read(ishikawa_pref_file, quiet = TRUE) |>
  # 能登地方（宝達志水町以北）の市町を抽出します。
  dplyr::filter(N03_004 %in% c("輪島市", "珠洲市", "穴水町",
                               "能登町", "七尾市", "羽咋市",
                               "志賀町", "宝達志水町",
                               "中能登町")) |>
  sf::st_union()

# 土地利用データのうち能登地方に重なる部分を抽出
int <- sf::st_intersects(landuse, noto, sparse = FALSE)
landuse_noto <- landuse[int, ]

# クマ出没データについて、3次メッシュコードと年ごとに
# 出没の有無と件数を集計
p_sum <- kuma_data |>
  dplyr::rename(year = `出没年`) |>
  dplyr::group_by(mesh_code, year) |>
  dplyr::summarise(present = as.numeric(n() > 0),
                   n = n(),
                   .groups = "drop")

# 土地利用データから、3次メッシュコードと地理情報だけの
# データを作成
geometry <- landuse_noto |>
  dplyr::select(mesh_code, geometry)

# 3次メッシュコードごとに、6年間の出没ありの年の割合を計算
prop <- p_sum |>
  dplyr::group_by(mesh_code) |>
  dplyr::summarise(prop = n() / 6, .groups = "drop")

# 3次メッシュコードごとの、6年間の出没ありの年の割合を
# 地図として可視化
geometry |>
  dplyr::left_join(prop, by = "mesh_code") |>
  dplyr::mutate(prop = replace_na(prop, 0)) |>
  ggplot(aes(fill = prop)) +
  geom_sf() +
  scale_fill_viridis_c(name = "割合") +
  labs(title = "2019〜2024年の能登地方のクマ出没ありの年の割合") +
  theme_minimal(base_family = "Noto Sans JP")

# 土地利用との関係
land_prop <- landuse_noto |>
  dplyr::left_join(prop, by = "mesh_code") |>
  tidyr::replace_na(list(n = 0, prop = 0))
ggplot(land_prop, aes(x = `森林`, y = prop)) +
  geom_point(color = "red", size = 2.5) +
  labs(title = "能登地方における、3次メッシュ中の森林面積とクマ出没頻度との関係",
       x = expression(paste("森林面積(", km^2, ")")),
       y = "出没があった年の割合") +
  theme_bw(base_family = "Noto Sans JP")

ggplot(land_prop, aes(x = `河川地及び湖沼`, y = prop)) +
  geom_point(color = "red", size = 2.5) +
  labs(title = "能登地方における、河川地及び湖沼面積とクマ出没頻度との関係",
       x = expression(paste("河川地及び湖沼面積(", km^2, ")")),
       y = "出没があった年の割合") +
  theme_bw(base_family = "Noto Sans JP")

# ブナ豊凶との関係
tidyr::expand_grid(
  mesh_code = unique(landuse_noto$mesh_code),
  year = 2019:2024
) |>
  dplyr::left_join(p_sum, by = c("mesh_code", "year")) |>
  tidyr::replace_na(list(present = 0, n = 0)) |>
  dplyr::mutate(buna = if_else(year == 2020, "大凶作", "大凶作以外")) |>
  ggplot(aes(x = as.factor(year), y = n, color = buna)) +
  geom_jitter(size = 2.5, alpha = 0.5, height = 0, width = 0.05) +
  labs(title = "能登地方における、各年の3次メッシュごとの出没件数",
       x = "年",
       y = "出没件数") +
  scale_color_discrete(name = "ブナの豊凶") +
  theme_bw(base_family = "Noto Sans JP")


# クマ出没予測モデル

# モデルの説明変数として使用する可能性のある環境データの作成
# forest:  3次メッシュ中の森林面積(km^2^)
# forest2: 3次メッシュ中の森林面積(km^2^)の2乗
# water:   3次メッシュ中の河川地及び湖沼面積(km^2^)
# water2:  3次メッシュ中の河川地及び湖沼面積(km^2^)の2乗
env <- landuse_noto |>
  dplyr::mutate(forest = `森林`,
                forest2 = `森林`^2,
                water = `河川地及び湖沼`,
                water2 = `河川地及び湖沼`^2) |>
  as.data.frame() |>
  dplyr::select(mesh_code, forest, forest2, water, water2)

# 3次メッシュコードと年のすべての組み合わせをつくっておき、
# これに出没のありなしデータと環境データを結合
p_year <- tidyr::expand_grid(
  mesh_code = unique(landuse_noto$mesh_code),
  year = 2019:2024
) |>
  dplyr::left_join(p_sum, by = c("mesh_code", "year")) |>
  dplyr::mutate(present = replace_na(present, 0)) |>
  dplyr::left_join(env, by = "mesh_code") |>
  # ブナ豊凶をしめすダミー変数`buna_poor`を追加（大凶作=1）
  dplyr::mutate(buna_poor = if_else(year == 2020, 1, 0))

# sdmTMBで使用するメッシュ作成のため、UTM座標系の座標を追加
coord <- landuse_noto |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::rename(longitude = X, latitude = Y) |>
  add_utm_columns(ll_names = c("longitude", "latitude"))

# UTM座標に3次メッシュコードを結合
coord2 <- coord |>
  bind_cols(
    landuse_noto |>
      dplyr::select(mesh_code)
    )
# 出没あり・なしデータにUTM座標を結合
p_year_coord <- p_year |>
  dplyr::left_join(coord2, by = "mesh_code")

# メッシュの作成
mesh <- make_mesh(p_year_coord, xy_cols = c("X", "Y"), cutoff = 2)

# 作成されたメッシュの確認
plot(mesh)

# モデルの作成とあてはめ
# モデル1: ブナの豊凶(B)を説明変数とするモデル
# モデル2: 森林面積(F)とその2乗を説明変数とするモデル
# モデル3: 森林面積(F)とその2乗、ブナの豊凶(B)を説明変数とするモデル
# モデル4: 森林面積(F)とその2乗、河川・湖沼面積(W)、ブナの豊凶(B)を説明変数とするモデル
# ともに、これら説明変数のほかに、空間の変量効果をモデルに組み込んでいます。

fit1 <- sdmTMB(present ~ buna_poor,
              data = p_year_coord,
              mesh = mesh,
              family = binomial(link = "logit"),
              spatial = "on",
              time = "year",
              time_varying = ~ 1,
              time_varying_type = "ar1",
              extra_time = 2025)
fit2 <- sdmTMB(present ~ forest + forest2,
              data = p_year_coord,
              mesh = mesh,
              family = binomial(link = "logit"),
              spatial = "on",
              time = "year",
              time_varying = ~ 1,
              time_varying_type = "ar1",
              extra_time = 2025)
fit3 <- sdmTMB(present ~ forest + forest2 + buna_poor,
               data = p_year_coord,
               mesh = mesh,
               family = binomial(link = "logit"),
               spatial = "on",
               time = "year",
               time_varying = ~ 1,
               time_varying_type = "ar1",
               extra_time = 2025)
fit4 <- sdmTMB(present ~ forest + forest2 + water + buna_poor,
               data = p_year_coord,
               mesh = mesh,
               family = binomial(link = "logit"),
               spatial = "on",
               time = "year",
               time_varying = ~ 1,
               time_varying_type = "ar1",
               extra_time = 2025)

# モデル1のあてはめ結果のチェック
sanity(fit1)

# モデル2のあてはめ結果のチェック
sanity(fit2)

# モデル3のあてはめ結果のチェック
sanity(fit3)

# モデル4のあてはめ結果のチェック
sanity(fit4)

# いずれも問題は発見されませんでした。

# モデル1のあてはめ結果の要約
summary(fit1)

# モデル2のあてはめ結果の要約
summary(fit2)

# モデル3のあてはめ結果の要約
summary(fit3)

# モデル4のあてはめ結果の要約
summary(fit4)


# AICの比較
c(AIC(fit1), AIC(fit2), AIC(fit3), AIC(fit4))

# AICの値はモデル2の方がもっとも小さいので、
# より予測能力が高いモデルとしてモデル2を採用します。

# 2025年の出没確率の予測

# ブナ大凶作として2025年のデータを作成
newdata <- env |>
  dplyr::left_join(coord2, by = "mesh_code") |>
  dplyr::mutate(buna_poor = 1,
                year = 2025)

# モデル2を使用して2025年の出没確率を予測
pred_buna_poor <- predict(fit2, newdata, type = "response")

# 出没確率を地図化
pred_buna_poor |>
  sf::st_as_sf() |>
  ggplot(aes(fill = est)) +
  geom_sf() +
  scale_fill_viridis_c(name = "出没確率", limits = c(0, 1)) +
  labs(title = "2025年の能登地方のクマ出没確率の予測値") +
  theme_minimal(base_family = "Noto Sans JP")


# 2025年の出没予測確率と実際の出没データの比較

# 2025年の出没状況
pre_2025 <- file.path(data_dir, "original", "bear_sightings_r7.csv") |>
  readr::read_csv() |>
  dplyr::mutate(mesh_code = jpmesh::coords_to_mesh(`経度`, `緯度`, 1) |>
                  as.character()) |>
  dplyr::group_by(mesh_code) |>
  dplyr::summarise(n = n(), .groups = "drop")

# 予測と実際の出没データを結合
bind_2025 <- pred_buna_poor |>
  dplyr::left_join(pre_2025, by = "mesh_code") |>
  dplyr::mutate(pre_2025 = if_else(is.na(n), 0, 1))

# 横軸を出没確率、縦軸を出没の有無としてプロット
ggplot(bind_2025, aes(x = est, y = pre_2025)) +
  geom_jitter(color = "red", size = 3, alpha = 0.3,
              width = 0, height = 0.02) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = c(0, 1), minor_breaks = NULL) +
  labs(title = "2025年の出没予測確率と実際の出没状況",
       x = "2025年の出没予測確率", y = "実際の出没のあり(1)なし(0)") +
  theme_bw(base_family = "Noto Sans JP")

