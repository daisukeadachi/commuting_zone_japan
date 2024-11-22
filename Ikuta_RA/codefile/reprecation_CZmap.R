library(tidyverse)
library(sf)
library(RColorBrewer)
library(spdep)

### Data cleaning ##############################################################

CZ_2005 <- readr::read_csv("output/2005_original.csv")

CZ_2005.sf <- sf::read_sf("mapdata/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>%
  dplyr::left_join(CZ_2005, by = c("JISCODE" = "i")) %>% 
  sf::st_transform(4612)

# 沖縄県を日本列島の北東に動かす。
CZ_Okinawa <- CZ_2005.sf %>% 
  dplyr::filter(JISCODE %in% (47000:47999)) %>% 
  sf::st_set_geometry(st_geometry(CZ_2005.sf %>% dplyr::filter(JISCODE %in% (47000:47999))) + c(5, 15)) %>% #将来的？:c(17, ?)で日本列島の南側に移せる(北海道を移すスペースを確保できる)
  sf::st_set_crs(4612)

# 最終的なプロット用データの作成。
# 東京都小笠原村は単独のCZなので、地図の目的からすると描画する必要はないと判断。
CZ_2005.sf <- CZ_2005.sf %>%
  dplyr::filter(JISCODE != 13421, !(JISCODE %in% (47000:47999))) %>%
  dplyr::bind_rows(CZ_Okinawa)

# 沖縄県と日本列島とを区切る線
lineMatrix = base::rbind(c(138, 45), c(138, 40), c(130, 37))
OkinawaLine <- st_linestring(lineMatrix) %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4612)


## カスタムパレットで塗り分ける色の指定 ########################################

sf_use_s2(FALSE) # s2ジオメトリエンジンを無効にする

CZ_2005.sf.temp <- CZ_2005.sf %>%
  group_by(cluster) %>%
  select(cluster) %>%
  summarise() %>%
  sf::st_make_valid()

# 隣接関係の計算
neighbors <- spdep::poly2nb(CZ_2005.sf.temp)
neighbor_matrix <- spdep::nb2mat(neighbors, style = "B", zero.policy = TRUE)

# グラフ彩色アルゴリズムの適用
colors <- RColorBrewer::brewer.pal(8, "Set2")
color_assignment <- rep(NA, length(neighbors))

for (i in 1:length(neighbors)) {
  available_colors <- setdiff(colors, color_assignment[neighbors[[i]]])
  color_assignment[i] <- available_colors[1]
}
CZ_2005.sf.temp$color <- color_assignment

CZ_2005.temp <- CZ_2005.sf.temp %>%
  dplyr::tibble() %>%
  select(-geometry)

### 地図のプロット #############################################################

sf_use_s2(TRUE)

CZ_2005.sf %>%
  dplyr::left_join(CZ_2005.temp, by = "cluster") %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_sf(data = OkinawaLine) +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(title = "CZ_alt") -> CZmap_2005

# CZ_2005.sf %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(aes(fill = fill)) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::coord_sf(ylim = c(34, 37),
#                     xlim = c(138, 141),
#                     datum = NA) +
#   ggplot2::labs(title = "関東地方・Commuting Zone(2005)")+
#   ggplot2::theme(plot.title    = element_text(size = 10)) -> CZmap_2005_Kanto
# 
ggplot2::ggsave(filename = "output/mapB.png", bg = "white")
# 
