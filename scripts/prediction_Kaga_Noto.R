#
# 石川県加賀地方クマ出没予測マップの作成
#

library(tidyverse)
library(sf)
library(nanoparquet)
library(patchwork)
library(sdmTMB)

# データ配置ディレクトリ
data_dir <- "data"

# 出力ファイルのディレクトリ
output_dir <- "outputs"

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

# 擬似乱数のシードの設定
set.seed(1234)

# クマ出没データの読み込み
kuma_data <- read_parquet(data_file) |>
  # 500mメッシュコードを3次(1km)メッシュコードに変換
  dplyr::mutate(mesh_code = str_sub(`500mメッシュコード`, 1, 8))

# 土地利用データの読み込み
landuse_data <- landuse_files %>%
  purrr::map(\(f) {
    sf::st_read(f, quiet = TRUE)
  }) |>
  dplyr::bind_rows() |>
  # m^2単位をkm^2単位にする
  dplyr::mutate_at(2:14, `/`, 10^6) |>
  # 3次メッシュコードを文字型にする
  dplyr::mutate(mesh_code = as.character(`メッシュ`))

# 地域名の定義
region <- c("Kaga", "Noto")

# 石川県の行政区域データの読み込み
ishikawa <- sf::st_read(ishikawa_pref_file, quiet = TRUE)

# 各地域の市町名を格納するリスト
city <- vector("list", 2)

# 加賀地方（かほく市以南）の市町を抽出
city[["Kaga"]] <- ishikawa |>
  dplyr::filter(N03_004 %in% c("金沢市", "小松市", "加賀市", "かほく市",
                               "白山市", "能美市", "野々市市", "川北町",
                               "津幡町", "内灘町")) |>
  sf::st_union()

# 能登地方（宝達志水町以北）の市町を抽出
city[["Noto"]] <- ishikawa |>
  dplyr::filter(N03_004 %in% c("輪島市", "珠洲市", "穴水町",
                               "能登町", "七尾市", "羽咋市",
                               "志賀町", "宝達志水町",
                               "中能登町")) |>
  sf::st_union()


# クマ出没の有無と回数を3次メッシュと年ごとに集計
kuma_sum <- kuma_data |>
  dplyr::rename(year = `出没年`) |>
  dplyr::group_by(mesh_code, year) |>
  dplyr::summarise(present = as.numeric(n() > 0),
                   n = n(),
                   .groups = "drop")

# 3次メッシュ土地利用データを各地域ごとに抽出
landuse <- purrr::map(
  region, \(r) {
    intersects <- sf::st_intersects(landuse_data, city[[r]], sparse = FALSE)
    landuse_data[intersects, ]
})
names(landuse) <- region

# 土地利用データから3次メッシュコードと地理情報だけを抽出
geometry <- purrr::map(
  region, \(r) {
    landuse[[r]] |>
      dplyr::select(mesh_code, geometry)
})
names(geometry) <- region

# 3次メッシュごとに、クマ出没情報があった年の割合を計算
prop_app <- kuma_sum |>
  dplyr::group_by(mesh_code) |>
  dplyr::summarise(prop = n() / 6, .groups = "drop")

# クマ出没情報があった年の割合を地図にプロットする関数を定義
plot_prop <- function(region, title) {
  geometry[[region]] |>
  dplyr::left_join(prop_app, by = "mesh_code") |>
  dplyr::mutate(prop = replace_na(prop, 0)) |>
  ggplot(aes(fill = prop)) +
  geom_sf() +
  scale_fill_viridis_c(name = "割合", limits = c(0, 1)) +
  labs(title = title) +
  theme_minimal(base_family = "Noto Sans JP")
}

# 能登地方と加賀地方の、3次メッシュごとのクマの出没情報があった年の割合
#（2019〜2024年）をプロット
p1 <- plot_prop("Kaga", "加賀地方")
p2 <- plot_prop("Noto", "能登地方")
p2 / p1 + plot_layout(guides = "collect")

# 土地利用とクマ出没のあった年の割合の関係をプロットする関数を定義
plot_landuse_prop <- function(region, type,
                              title = NULL, xlab = NULL,
                              ylab = "割合") {
  landuse[[region]] |>
  dplyr::left_join(prop_app, by = "mesh_code") |>
  tidyr::replace_na(list(n = 0, prop = 0)) |>
  ggplot(aes(x = .data[[type]], y = prop)) +
  geom_point(color = "red", size = 2.5) +
  geom_smooth(method = "gam") +
  labs(title = title, x = xlab, y = ylab) +
  theme_bw(base_family = "Noto Sans JP")
}

# 加賀地方における、3次メッシュ中の森林面積と
# クマ出没のあった年の割合との関係
plot_landuse_prop("Kaga", "森林",
                  x = expression(paste("森林面積(", km^2, ")")),
                  title = paste0("加賀地方における、森林面積と",
                                 "クマ出没のあった年の割合との関係"))

# 加賀地方における、3次メッシュ中の河川地及び湖沼面積と
# クマ出没のあった年の割合との関係
plot_landuse_prop("Kaga", "河川地及び湖沼",
                  x = expression(paste("河川地及び湖沼面積(", km^2, ")")),
                  title = paste0("加賀地方における、河川地及び湖沼面積と",
                                 "クマ出没のあった年の割合との関係"))


# 能登地方における、3次メッシュ中の森林面積と
# クマ出没のあった年の割合との関係
plot_landuse_prop("Noto", "森林",
                  x = expression(paste("森林面積(", km^2, ")")),
                  title = paste0("能登地方における、森林面積と",
                                 "クマ出没のあった年の割合との関係"))


# 能登地方における、3次メッシュ中の河川地及び湖沼面積と
# クマ出没のあった年の割合との関係
plot_landuse_prop("Noto", "河川地及び湖沼",
                  x = expression(paste("河川地及び湖沼面積(", km^2, ")")),
                  title = paste0("能登地方における、河川地及び湖沼面積と",
                                 "クマ出没のあった年の割合との関係"))

# ブナの豊凶も可視化しつつ、各年のクマ出没件数の関係をプロットする関数を定義
plot_buna_freq <- function(region, title = NULL,
                           xlab = "年", ylab = "出没件数") {
  tidyr::expand_grid(
    mesh_code = unique(landuse[[region]]$mesh_code),
    year = 2019:2024
  ) |>
    dplyr::left_join(kuma_sum, by = c("mesh_code", "year")) |>
    tidyr::replace_na(list(present = 0, n = 0)) |>
    dplyr::mutate(buna = if_else(year == 2020, "大凶作", "大凶作以外")) |>
    ggplot(aes(x = as.factor(year), y = n, color = buna)) +
    geom_jitter(size = 2.5, alpha = 0.5, height = 0, width = 0.05) +
    labs(title = title,  x = xlab, y = ylab) +
    scale_color_discrete(name = "ブナの豊凶") +
    theme_bw(base_family = "Noto Sans JP")
}

# 加賀地方における、各年の3次メッシュごとのクマ出没件数（赤丸は大凶作の年）
plot_buna_freq("Kaga")

# 能登地方における、各年の3次メッシュごとのクマ出没件数（赤丸は大凶作の年）
plot_buna_freq("Noto")

# 環境データの作成
env_data <- purrr::map(
  region, \(r) {
    landuse[[r]] |>
      dplyr::mutate(forest = `森林`,
                    forest2 = `森林`^2,
                    water = `河川地及び湖沼`,
                    water2 = `河川地及び湖沼`^2) |>
      as.data.frame() |> # drop geometry
      dplyr::select(mesh_code, forest, forest2, water, water2)
  })
names(env_data) <- region

# クマ出没データと環境データを結合
kuma_env_data <- purrr::map(
  region, \(r) {
    tidyr::expand_grid(
      mesh_code = unique(landuse[[r]]$mesh_code),
      year = 2019:2024
    ) |>
      dplyr::left_join(kuma_sum, by = c("mesh_code", "year")) |>
      dplyr::mutate(present = replace_na(present, 0),
                    n = replace_na(n, 0)) |>
      dplyr::left_join(env_data[[r]], by = "mesh_code") |>
      # ブナ豊凶をしめすダミー変数`buna_poor`を追加（大凶作=1）
      dplyr::mutate(buna_poor = if_else(year == 2020, 1, 0))
  })
names(kuma_env_data) <- region

# 座標変換関数の定義
coord <- purrr::map(
  region, \(r) {
    landuse[[r]] |>
      sf::st_transform(4326) |> # WGS84
      sf::st_make_valid() |>
      sf::st_centroid() |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::rename(longitude = X, latitude = Y) |>
      add_utm_columns(ll_names = c("longitude", "latitude"))
  })
names(coord) <- region

# 座標データに3次メッシュコードを結合
coord2 <- purrr::map(
  region, \(r) {
    coord[[r]] |>
      bind_cols(
        landuse[[r]] |>
          dplyr::select(mesh_code)
      )
  })
names(coord2) <- region

# クマ・環境データに座標データを結合
kuma_env_coord <- purrr::map(
  region, \(r) {
    kuma_env_data[[r]] |>
      dplyr::left_join(coord2[[r]], by = "mesh_code")
  })
names(kuma_env_coord) <- region

# sdmTMB用のメッシュを作成
mesh <- purrr::map(
  region, \(r) {
    make_mesh(kuma_env_coord[[r]],
              xy_cols = c("X", "Y"), cutoff = 2)
  })
names(mesh) <- region

# 加賀地方のメッシュを確認
plot(mesh[["Kaga"]])

# 能登地方のメッシュを確認
plot(mesh[["Noto"]])

# sdmTMBのモデル式を定義
# 5種類のモデル
formula <- vector("list", 5)
formula[[1]] <- as.formula("present ~ buna_poor")
formula[[2]] <- as.formula("present ~ forest + forest2")
formula[[3]] <- as.formula("present ~ forest + forest2 + buna_poor")
formula[[4]] <- as.formula(paste("present ~ forest + forest2 +",
                                 "water + water2"))
formula[[5]] <- as.formula(paste("present ~ forest + forest2 +",
                                 "water + water2 + buna_poor"))

# 保存したあてはめ結果があれば読み込む
fit_file <- file.path(output_dir, "kuma_sdmTMB_fit.rds")
if (file.exists(fit_file)) {
  fit <- readRDS(fit_file)
} else {
  # ファイルがなければ、あてはめ実行

  # あてはめた結果を格納するリストを作成
  fit <- vector("list", 2)
  names(fit) <- region

  # 加賀地方についてあてはめ実行
  fit[["Kaga"]] <- purrr::map(
    1:5, \(i) {
      sdmTMB(formula[[i]],
             data = kuma_env_coord[["Kaga"]],
             mesh = mesh[["Kaga"]],
             family = binomial(link = "logit"),
             spatial = "on",
             time = "year",
             time_varying = ~ 1,
             time_varying_type = "ar1",
             extra_time = 2025)
    })
 
  # 能登地方についてあてはめ実行
  fit[["Noto"]] <- purrr::map(
    1:5, \(i) {
      sdmTMB(formula[[i]],
             data = kuma_env_coord[["Noto"]],
             mesh = mesh[["Noto"]],
             family = binomial(link = "logit"),
             spatial = "on",
             time = "year",
             time_varying = ~ 1,
             time_varying_type = "ar1",
             extra_time = 2025)
    })
  
  # あてはめ結果を保存
  saveRDS(fit, fit_file)
}

# 加賀地方のあてはめ結果を確認
# 結果の診断
purrr::walk(1:5, \(i) {
  cat(paste("model:", i, "\n"))
  sdmTMB::sanity(fit[["Kaga"]][[i]])
})

# 各モデルのAICを表示
purrr::walk(1:5, \(i) {
  print(paste0("モデル", i, "のAIC: ", AIC(fit[["Kaga"]][[i]])))
})

# もっともAICがちいさかったモデル5の結果の要約を表示
summary(fit[["Kaga"]][[5]])

# 能登地方のあてはめ結果を確認
# 結果の診断
purrr::walk(1:5, \(i) {
  cat(paste("model:", i, "\n"))
  sdmTMB::sanity(fit[["Noto"]][[i]])
})

# 各モデルのAICを表示
purrr::walk(1:5, \(i) {
  print(paste0("モデル", i, "のAIC: ", AIC(fit[["Noto"]][[i]])))
})

# もっともAICが小さかったモデル2と次に小さかったモデル3の
# 結果の要約を表示
summary(fit[["Noto"]][[2]])
summary(fit[["Noto"]][[3]])


# 2025年のクマ出没確率を予測

# 予測結果を格納するリストを作成
pred <- vector("list", 2)
names(pred) <- region

# 予測データと予測値のリストを作成
newdata <- vector("list", 2)
names(newdata) <- region
pred <- vector("list", 2)
names(pred) <- region

# 加賀地方の予測
# 2025年のデータを作成
newdata[["Kaga"]] <- env_data[["Kaga"]] |>
  dplyr::left_join(coord2[["Kaga"]], by = "mesh_code") |>
  dplyr::mutate(buna_poor = 1,
                year = 2025)

# モデル5を使用してpredictで予測実行
est <- predict(fit[["Kaga"]][[5]], newdata[["Kaga"]],
               type = "response", nsim = 2000)

# 予測値として上側20%点を使用
upper <- apply(est, 1, quantile, 0.8) # 上側20%点
pred[["Kaga"]] <- newdata[["Kaga"]] |>
  dplyr::mutate(pred = upper) |>
  dplyr::select(mesh_code, pred, geometry)

# 2025年の加賀地方のクマ出没確率の予測マップ
pred[["Kaga"]] |>
  sf::st_as_sf() |>
  ggplot(aes(fill = pred)) +
  geom_sf() +
  scale_fill_viridis_c(name = "出没確率", limits = c(0, 1)) +
  theme_minimal(base_family = "Noto Sans JP")

# 能登地方の予測
# 2025年のデータを作成
newdata[["Noto"]] <- env_data[["Noto"]] |>
  dplyr::left_join(coord2[["Noto"]], by = "mesh_code") |>
  dplyr::mutate(buna_poor = 1,
                year = 2025)

# AICはモデル2の方が小さかったのですが、ブナ大凶作の影響も含めた
# モデル3を使用してpredictで予測実行
est <- predict(fit[["Noto"]][[3]], newdata[["Noto"]],
               type = "response", nsim = 2000)

# 予測値として上側20%点を使用
upper <- apply(est, 1, quantile, 0.8) # 上側20%点
pred[["Noto"]] <- newdata[["Noto"]] |>
  dplyr::mutate(pred = upper) |>
  dplyr::select(mesh_code, pred, geometry)

# 2025年の能登地方のクマ出没確率の予測マップ
pred[["Noto"]] |>
  sf::st_as_sf() |>
  ggplot(aes(fill = pred)) +
  geom_sf() +
  scale_fill_viridis_c(name = "出没確率", limits = c(0, 1)) +
  theme_minimal(base_family = "Noto Sans JP")

# 加賀地方と能登地方の予測結果を結合し、重複する3次メッシュコードについては
# 出没確率の平均をとる
pred_all <- bind_rows(pred)
dup <- pred_all |>
  dplyr::count(mesh_code) |>
  dplyr::filter(n > 1)
pred_dup <- pred_all |>
  dplyr::filter(mesh_code %in% dup$mesh_code) |>
  dplyr::group_by(mesh_code) |>
  dplyr::summarise(pred = mean(pred), .groups = "drop")

# 重複を整理し、石川県全体の3次メッシュごとの
# 2025年のクマ出没確率の予測データを作成
combinded_pred <- pred_all |>
  dplyr::left_join(pred_dup, by = "mesh_code") |>
  dplyr::mutate(pred = if_else(is.na(pred.y), pred.x, pred.y)) |>
  dplyr::distinct(mesh_code, .keep_all = TRUE) |>
  dplyr::select(mesh_code, pred, geometry) |>
  sf::st_as_sf()

# 統合した石川県全体のクマ出没予測確率のマップ
ggplot(combinded_pred, aes(fill = pred)) +
  geom_sf() +
  scale_fill_viridis_c(name = "出没確率", limits = c(0, 1)) +
  theme_minimal(base_family = "Noto Sans JP")

# 予測結果をGeoJSON形式で保存
output_file <- file.path(data_dir, "kuma_prediction.geojson")
if (file.exists(output_file))
  file.remove(output_file)
sf::st_write(combinded_pred, output_file)

# 2025年の観測データを用いて予測結果の検証
occ_2025 <- file.path(data_dir, "original",  "bear_sightings_r7.csv") |>
  readr::read_csv() |>
  dplyr::mutate(mesh_code = jpmesh::coords_to_mesh(`経度`, `緯度`, 1) |>
                  as.character()) |>
  dplyr::group_by(mesh_code) |>
  dplyr::summarise(n = n(), .groups = "drop")

# 2025年の観測データと予測結果を結合
bind_2025 <- combinded_pred |>
  dplyr::left_join(occ_2025, by = "mesh_code") |>
  dplyr::mutate(occ_2025 = if_else(is.na(n), 0, 1))

# 出没予測確率区分ごとに観測された出没ありの割合を表示
breaks <- seq(0, 1, 0.2)
n_breaks <- length(breaks)

prop_2025 <- bind_2025 |>
  dplyr::mutate(pred_cat = cut(pred, breaks,
                              include.lowest = TRUE)) |>
  dplyr::group_by(pred_cat) |>
  dplyr::summarise(n = n(),
                   n_present = sum(occ_2025),
                   prop_present = n_present / n,
                   .groups = "drop") |>
  dplyr::mutate(mid = (breaks[2:n_breaks] +
                         breaks[1:(n_breaks - 1)]) / 2)

ggplot(prop_2025, aes(x = mid, y = prop_present)) +
  geom_col(fill = "red", alpha = 0.7) +
  scale_x_continuous(limits = c(min(breaks), max(breaks)),
                     breaks = breaks, minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = breaks) +
  labs(x = "2025年の出没予測確率区分",
       y = "出没ありの割合") +
  theme_bw(base_family = "Noto Sans JP")

