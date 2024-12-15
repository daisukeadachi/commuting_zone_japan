library(tidyverse)
library(sf)
library(RColorBrewer)
library(spdep)
"%not.in%" <- Negate("%in%")
print("データは****2005年****のものです。正しくファイルが指定されているか確認してください。")
#北海道は動かしません

### Data cleaning ##############################################################

muni_map <- sf::read_sf("mapdata/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>% 
  dplyr::select(-NO, -DATE) %>% 
  dplyr::filter(JISCODE %not.in% c(1695, 1696, 1698, 13421)) %>%
  sf::st_transform(4612)

Railroad <- sf::read_sf("data/N05-23_GML/N05-23_RailroadSection2.shp") %>% 
  # 211行目までが北海道
  dplyr::rename(ID = N05_006) %>% 
  dplyr::mutate(constructed = as.integer(N05_004),
                start = as.integer(N05_005b),
                end = as.integer(N05_005e)) %>% 
  dplyr::select(ID, constructed, start, end) %>% 
  dplyr::filter(end >= 2005,
                start <= 2005) %>% 
  sf::st_transform(4612)

# Hokkaido <- muni_map %>%
#   dplyr::filter(JISCODE %in% (1000:1999)) %>%
#   sf::st_set_geometry(st_geometry(.) - c(10, 4)) %>%
#   sf::st_set_crs(4612)
# muni_map <- muni_map %>%
#   # dplyr::filter(!(JISCODE %in% (1000:1999))) %>%
#   dplyr::bind_rows(Hokkaido)
# Hokkaido.rail <- Railroad %>% 
#   dplyr::filter(st_coordinates(Railroad)[,2] <= 41.6) %>% 
#   # sf::st_set_geometry(st_geometry(.) - c(10, 4)) %>%
#   sf::st_set_crs(4612)
# Railroad <- dplyr::bind_rows(Railroad, Hokkaido.rail)

CZ_2005 <- readr::read_csv("output/2005_original.csv")
CZ_2005.sf <- muni_map %>% 
  dplyr::left_join(CZ_2005, by = c("JISCODE" = "i")) %>%
  sf::st_transform(4612)



## カスタムパレットで塗り分ける色の指定 ########################################

sf_use_s2(FALSE) # s2ジオメトリエンジンを無効にする

CZ_2005.color <- CZ_2005.sf %>%
  group_by(cluster) %>%
  select(cluster) %>%
  summarise() %>%
  sf::st_make_valid()


neighbors <- spdep::poly2nb(CZ_2005.color)

colors <- RColorBrewer::brewer.pal(8, "Set2")
color_assignment <- rep(NA, length(neighbors))

for (i in 1:length(neighbors)) {
  available_colors <- setdiff(colors, color_assignment[neighbors[[i]]])
  color_assignment[i] <- available_colors[1]
}
CZ_2005.color$color <- color_assignment
CZ_2005.color <- CZ_2005.color %>%
  dplyr::tibble() %>%
  select(-geometry)

CZ_2005.sf <- CZ_2005.sf %>% 
  dplyr::left_join(CZ_2005.color, by = "cluster") 

### 地図のプロット #############################################################

lineMatrix = base::rbind(c(137.5, 45), c(137.5, 40), c(134, 37), c(120, 37))
HokkaidoLine <- st_linestring(lineMatrix) %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4612)


sf_use_s2(TRUE)

# CZ_2005.sf %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
#   ggplot2::scale_fill_manual(values = colors) +
#   ggplot2::geom_sf(data = Railroad, color = "black", linewidth = 0.1, alpha = .8) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(legend.position = "none") +
#   # ggplot2::geom_sf(data = HokkaidoLine) +
#   ggplot2::coord_sf(ylim = c(31.2, 45.5),
#                     xlim = c(129.3, 145.8),
#                     datum = NA) +
#   ggplot2::labs(title = "CZ_alt",
#                 caption = "note:Ogasawara Village, Tokyo Prefecture is elminated from the map for the visiblity.
#                 (There no other municiparities included in the same CZ as Ogasawara Village)") -> CZmap_2005

CZ_2005.sf %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = color), color = "white") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::geom_sf(data = Railroad, color = "black", linewidth = .2, alpha = .8) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(ylim = c(34.6, 37.1),
                    xlim = c(138, 141),
                    datum = NA) +
  ggplot2::labs(title = "関東地方・Commuting Zone(2005)")+
  ggplot2::theme(plot.title    = element_text(size = 10)) -> CZmap_2005_Kanto

ggplot2::ggsave(filename = "cover/map/temp.png", bg = "white")



library(beepr)
beep()



