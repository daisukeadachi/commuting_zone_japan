library(tidyverse)
library(sf)
library(gridExtra)
library(spdep)
library(RColorBrewer)

timestamp()

# UEA data --------------------------------------------------------------------- 

##McEA(中小規模の都市圏)

McEA_2005 <- readr::read_csv("data/McEA2005.csv", locale = locale(encoding = "cp932")) %>% 
  dplyr::rename(UEA_Name = 3,
                suburb_name = 6,
                suburb_name2 = 10,
                suburb_name3 = 14) # 列名に何故かスペースを使っていたのでrename

McEA_2005C <- readr::read_csv("data/McEA2005C.csv", locale = locale(encoding = "cp932")) %>% 
  #中心市町村(Center)のリスト
  dplyr::rename(MEA_name = 3,
                Center_name = 6,
                DID_Population = 7,
                Center_DID_Population = 8,
                Employment_Residence_Ratio = 9)

##MEA(大規模都市圏)

MEA_2005 <- readr::read_csv("data/MEA2005.csv", locale = locale(encoding = "cp932")) %>% 
  dplyr::rename(UEA_Name = 3,
                suburb_name = 6,
                suburb_name2 = 10,
                suburb_name3 = 14) # 列名にスペース?を使っていたのでrename


MEA_2005C <- readr::read_csv("data/MEA2005C.csv", locale = locale(encoding = "cp932")) %>% 
  dplyr::rename(MEA_name = 3,
                Center_name = 6,
                DID_Population = 7,
                Center_DID_Population = 8,
                Employment_Residence_Ratio = 9)


# n次郊外を縦につなげる

McEA_sub3 <- McEA_2005 %>% 
  dplyr::select(UEA, 都市圏名, UEA_Name, suburb3, 郊外3, suburb_name3, suburb2) %>% 
  tidyr::drop_na(suburb3) %>% 
  dplyr::rename(JISCODE = suburb3,
                name_j = 郊外3,
                name_e = suburb_name3,
                depend_sub = suburb2) %>% 
  dplyr::mutate(order_suburb = "3")

McEA_sub2 <- McEA_2005 %>% 
  dplyr::select(UEA, 都市圏名, UEA_Name, suburb2, 郊外2, suburb_name2, suburb) %>% 
  tidyr::drop_na(suburb2) %>% 
  dplyr::rename(JISCODE = suburb2,
                name_j = 郊外2,
                name_e = suburb_name2,
                depend_sub = suburb) %>% 
  dplyr::mutate(order_suburb = "2")

McEA_sub1 <- McEA_2005 %>% 
  dplyr::select(UEA, 都市圏名, UEA_Name, suburb, 郊外, suburb_name) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(JISCODE = suburb,
                name_j = 郊外,
                name_e = suburb_name) %>% 
  dplyr::mutate(order_suburb = "1")

McEA_center <- McEA_2005C %>% 
  dplyr::select(UEA, 都市圏名, MEA_name, center, 中心都市, Center_name) %>% 
  dplyr::rename(UEA_Name = MEA_name,
                JISCODE = center,
                name_j = 中心都市,
                name_e = Center_name)

MEA_sub3 <- MEA_2005 %>% 
  dplyr::select(MEA, 都市圏名, UEA_Name, suburb3, 郊外3, suburb_name3, suburb2) %>% 
  tidyr::drop_na(suburb3) %>% 
  dplyr::rename(UEA = MEA,
                JISCODE = suburb3,
                name_j = 郊外3,
                name_e = suburb_name3,
                depend_sub = suburb2) %>% 
  dplyr::mutate(order_suburb = "3")

MEA_sub2 <- MEA_2005 %>% 
  dplyr::select(MEA, 都市圏名, UEA_Name, suburb2, 郊外2, suburb_name2, suburb) %>% 
  tidyr::drop_na(suburb2) %>% 
  dplyr::rename(UEA = MEA,
                JISCODE = suburb2,
                name_j = 郊外2,
                name_e = suburb_name2,
                depend_sub = suburb) %>% 
  dplyr::mutate(order_suburb = "2")

MEA_sub1 <- MEA_2005 %>% 
  dplyr::select(MEA, 都市圏名, UEA_Name, suburb, 郊外, suburb_name) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(UEA = MEA,
                JISCODE = suburb,
                name_j = 郊外,
                name_e = suburb_name) %>% 
  dplyr::mutate(order_suburb = "1")

MEA_center <- MEA_2005C %>% 
  dplyr::select(MEA, 都市圏名, MEA_name, center, 中心都市, Center_name) %>% 
  dplyr::rename(UEA = MEA,
                UEA_Name = MEA_name,
                JISCODE = center,
                name_j = 中心都市,
                name_e = Center_name)

UEA_2005 <- dplyr::bind_rows(McEA_center, McEA_sub1, McEA_sub2, McEA_sub3,
                             MEA_center,  MEA_sub1,  MEA_sub2,  MEA_sub3) %>% 
  dplyr::select(-name_j, -name_e)

base::rm(McEA_center, McEA_sub1, McEA_sub2, McEA_sub3,
         MEA_center,  MEA_sub1,  MEA_sub2,  MEA_sub3,
         McEA_2005, McEA_2005C, MEA_2005, MEA_2005C)



# CZ data ----------------------------------------------------------------------

CZ_2005 <- readr::read_csv("output/2005_original.csv") %>% 
  dplyr::rename(JISCODE = i)




# prepare map data -------------------------------------------------------------

muni_map <- sf::read_sf("mapdata/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>% 
  dplyr::select(-NO, -DATE) %>% 
  sf::st_transform(4612)

# # 北海道･本州･四国･九州以外の離島は地図に出さない
# sf_use_s2(FALSE) # s2ジオメトリエンジンを無効化(ポリゴンの統合や隣接行列の計算のため)
# 
# 
# muni_neighbors <- spdep::poly2nb(muni_map) %>% 
#   spdep::nb2mat(style = "B", zero.policy = TRUE) %>% 
#   base::rowSums()
# muni_map$neighbors <- muni_neighbors
# 
# island_munis <- c(1518, 1519, 28226, 28205, 28224, 28685, 37321, 37323, 37322,
#                   37364, 34206, 34430, 46213, 43212, 43527, 43523, 43207, 43530,
#                   43531 ,43532 , 43533, 43209, 46403, 46404, 46207)
# 
# muni_map <- muni_map %>% 
#   dplyr::filter(neighbors != 0 ,
#                 !(JISCODE %in% (46501:47999)),
#                 !(JISCODE %in% island_munis)) 
# 
# sf_use_s2(TRUE) 

# 北海道を動かす
Hokkaido <- muni_map %>% 
  dplyr::filter(JISCODE %in% (1000:1999)) %>% 
  sf::st_set_geometry(st_geometry(muni_map %>% dplyr::filter(JISCODE %in% (1000:1999))) - c(10, 4)) %>% 
  sf::st_set_crs(4612)

muni_map <- muni_map %>% 
  dplyr::filter(!(JISCODE %in% (1000:1999))) %>% 
  dplyr::bind_rows(Hokkaido)


CZ_map <- muni_map %>% 
  dplyr::left_join(CZ_2005, by = "JISCODE")

UEA_2005.sf <- muni_map %>% 
  dplyr::left_join(UEA_2005 , by = "JISCODE") 

  sf_use_s2(FALSE) 
  
# assign color to CZ/UEA -------------------------------------------------------

colors <- RColorBrewer::brewer.pal(8, "Set2")

#CZ
CZ_color <- CZ_map %>%
  dplyr::group_by(cluster) %>%
  dplyr::select(cluster) %>%
  dplyr::summarise() %>%
  sf::st_make_valid()

# 隣接関係の計算
neighbors <- spdep::poly2nb(CZ_color)
neighbor_matrix <- spdep::nb2mat(neighbors, style = "B", zero.policy = TRUE)

# 色の割り当て
color_assignment <- rep(NA, length(neighbors))

for (i in 1:length(neighbors)) {
  available_colors <- setdiff(colors, color_assignment[neighbors[[i]]])
  color_assignment[i] <- available_colors[1]
}
CZ_color$color <- color_assignment
CZ_color <- CZ_color %>%
  dplyr::tibble() %>%
  select(-geometry)

CZ_map <- CZ_map %>% 
  dplyr::left_join(CZ_color, by = "cluster")

base::rm(color_assignment, i, neighbors, neighbor_matrix, CZ_color)

#UEA
UEA_color <- UEA_2005.sf %>%
  tidyr::drop_na(UEA) %>% 
  dplyr::group_by(UEA) %>%
  dplyr::select(UEA) %>%
  dplyr::summarise() %>%
  sf::st_make_valid()

neighbors <- spdep::poly2nb(UEA_color)
neighbor_matrix <- spdep::nb2mat(neighbors, style = "B", zero.policy = TRUE)

color_assignment <- rep(NA, length(neighbors))

for (i in 1:length(neighbors)) {
  available_colors <- setdiff(colors, color_assignment[neighbors[[i]]])
  color_assignment[i] <- available_colors[1]
}
UEA_color$color <- color_assignment
UEA_color_ <- UEA_color %>%
  dplyr::tibble() %>%
  select(-geometry)

UEA_2005.sf <- UEA_2005.sf %>% 
  dplyr::left_join(UEA_color_, by = "UEA")

base::rm(color_assignment, i, neighbors, neighbor_matrix, UEA_color)

sf_use_s2(TRUE) 

# plot map ---------------------------------------------------------------------

lineMatrix = base::rbind(c(139.5, 41), c(137.5, 40), c(137.5, 38), c(134, 37), c(130, 37))
HokkaidoLine <- st_linestring(lineMatrix) %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4612)


UEA_2005.sf %>% 
  sf::st_set_crs(4612) %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "grey") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_sf(data = HokkaidoLine) +
  ggplot2::coord_sf(ylim = c(31.2, 42),
                    xlim = c(129.3, 142.3),
                    datum = NA) +
  ggplot2::labs(title = "都市雇用圏(UEA).2005")+
  theme(plot.title    = element_text(size = 8))　-> UEAmap_2005

UEA_2005.sf %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_sf(aes(fill = color)) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(ylim = c(34.5, 37.1),
                    xlim = c(138, 141),
                    datum = NA) +
  ggplot2::labs(title = "関東地方・UEA(2005)")+
  theme(plot.title    = element_text(size = 8))　-> UEAmap_2005_Kanto

CZ_map %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_sf(data = HokkaidoLine) +
  ggplot2::coord_sf(ylim = c(31.2, 42),
                    xlim = c(129.3, 142.3),
                    datum = NA) +
  ggplot2::labs(title = "Commuting Zone(2005)")+
  theme(plot.title    = element_text(size = 8)) -> CZmap_2005

CZ_map %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = color)) +
  ggplot2::scale_fill_manual(values = colors) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(ylim = c(34.5, 37.1),
                    xlim = c(138, 141),
                    datum = NA) +
  ggplot2::labs(title = "関東地方・Commuting Zone(2005)")+
  theme(plot.title    = element_text(size = 8)) -> CZmap_2005_Kanto

gridExtra::grid.arrange(UEAmap_2005, CZmap_2005, nrow = 1) %>% 
  ggplot2::ggsave(filename = "output/map_image/fetured/2005_UEA&CZmap.png", bg = "white", width = 5, height = 3)
gridExtra::grid.arrange(UEAmap_2005_Kanto, CZmap_2005_Kanto, nrow = 1) %>% 
  ggplot2::ggsave(filename = "output/map_image/fetured/2005_UEA&CZmap_kanto.png", bg = "white", width = 5, height = 3)

ggsave(UEAmap_2005, filename = "output/map_image/fetured/2005_UEAmap.png", bg = "white")
ggsave(UEAmap_2005_Kanto, filename = "output/map_image/fetured/2005_UEAmap_Kanto.png", bg = "white")
ggplot2::ggsave(CZmap_2005, filename = "output/map_image/fetured/2005_CZmap.png", bg = "white")
ggplot2::ggsave(CZmap_2005_Kanto, filename = "output/map_image/fetured/2005_CZmap_Kanto.png", bg = "white")


timestamp()

# #動的マップ作成---------------------------------------------------------------
# 
# CZ_2005wrep <- CZ_2005 %>%
#   dplyr::left_join(temp, by = "cluster")
# 
# UEAandCZ_2005 <-   muni_map %>%
#   dplyr::left_join(CZ_2005wrep, by = "JISCODE") %>%
#   dplyr::mutate(JISCODE = dplyr::if_else(stringr::str_sub(GNAME, -1, -1) == "市", base::trunc(JISCODE * 0.01) * 100 , JISCODE)) %>%
#   dplyr::mutate(JISCODE = dplyr::if_else((stringr::str_sub(CNAME, -1, -1) == "区" & PNAME == "東京都"), base::trunc(JISCODE * 0.01) * 100 , JISCODE)) %>%
#   dplyr::mutate(JISCODE = dplyr::if_else(GNAME %in% c("川崎市", "福岡市"), JISCODE + 30, JISCODE)) %>%
#   dplyr::full_join(UEA_2005 , by = "JISCODE") %>%
#   dplyr::rename(CZ = rep,
#                 UEA_NAME = 都市圏名)
# 
# library(leaflet)
# library(leafpop)
# 
# UEAandCZ_2005 %>%
#   mapview(zcol = c("CZ", "UEA_NAME"),
#           popup = popupTable(UEAandCZ_2005,
#                              zcol = c("UEA_NAME", "NAME")),
#           legend = FALSE)

# muni_map %>%
#   filter(neighbors != 0,
#          !(JISCODE %in% (46501:47999))) %>%
#   mapview::mapview()

