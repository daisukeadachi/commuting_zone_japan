pacman::p_load(
  tidyverse,
  sf,
  patchwork
)
"%not.in%" <- Negate("%in%")

CZ_year = 1980  # ここをいじるだけで下地のCZのマップをいじれる革新的コード
#### 下地のCZマップの準備 ####
colors <- RColorBrewer::brewer.pal(7, "Set1")
sf::sf_use_s2(FALSE)
mapPath <- paste0("mapdata/mmm", CZ_year, "1001/mmm", CZ_year, "1001.shp")
czPath <- paste0("output/", CZ_year, "_original.csv")
muni.sf <- sf::read_sf(mapPath, options = "ENCODING=CP932") %>% 
  dplyr::filter(JISCODE != 13421,
                JISCODE %not.in% c(1695, 1696, 1698)) %>% # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
  dplyr::select(-NO, -DATE) %>% 
  sf::st_transform(4612)
CZ.sf <- muni.sf %>% 
  dplyr::left_join(readr::read_csv(czPath), by = c("JISCODE" = "i"))
CZ_color <- CZ.sf %>%
  dplyr::group_by(cluster) %>%
  dplyr::select(cluster) %>%
  dplyr::summarise() %>%
  sf::st_make_valid()

EdoCenter <- which(CZ.sf$JISCODE == 13101)
Gohunai <- CZ.sf$cluster[EdoCenter]

neighbors <- spdep::poly2nb(CZ_color)
color_assignment <- rep(NA, length(neighbors))
color_assignment[Gohunai] <- colors[1]
roop <- (1:length(neighbors))[-Gohunai]

for (j in roop) {
  available_colors <- setdiff(colors, color_assignment[neighbors[[j]]])
  color_assignment[j] <- available_colors[1]
}
CZ_color$color <- color_assignment
CZ.sf <- CZ_color
rm(j, neighbors, color_assignment, available_colors, CZ_color, EdoCenter, Gohunai)

#### 鉄道 ####
Rail.row <- sf::read_sf("data/N06-23_GML/N06-23_HighwaySection.shp") %>% 
  dplyr::rename(ID = N06_004) %>% 
  dplyr::mutate(constructed = as.integer(N06_001),
                start = as.integer(N06_002),
                end = as.integer(N06_003)) %>% 
  dplyr::select(ID, constructed, start, end) %>% 
  sf::st_transform(4612)

#### map plot ####
year <- c(seq(1970, 2023, 10), 2023)
map_list <- list()
for (y in year){
  Rail_exist <- Rail.row %>% 
    dplyr::filter(start <= y & end >= y)
  CZ.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = .005, color = "gainsboro") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = Rail_exist, color = "black", linewidth = .05) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = y) +
    theme(plot.title    = element_text(size = 4)) -> temp
  map_list <- append(map_list, list(temp))
  rm(temp)
}
saveMap <- patchwork::wrap_plots(map_list, ncol = 3) +
  patchwork::plot_annotation(caption = paste0("CZは", CZ_year, "年のもの。"),
                             theme = theme(plot.caption = element_text(size = 4)))
#### save ####
ggplot2::ggsave(plot =saveMap, filename = paste0("output/map_image/Railroad/fixed_CZ/1950to2015_", CZ_year, "CZandExpwaymap.png"), dpi = 1200)

