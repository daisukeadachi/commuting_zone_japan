library(tidyverse)
library(sf)
library(spdep)
library(RColorBrewer)
library(leaflet)
library(leafpop)
library(mapview)
"%not.in%" <- Negate("%in%")

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
  dplyr::filter(JISCODE %not.in% c(1695, 1696, 1698, 13421)) %>%
  sf::st_transform(4612)

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
CZ_color$CZ_color <- color_assignment
CZ_color <- CZ_color %>%
  dplyr::tibble() %>%
  select(-geometry)

CZ_map <- CZ_map %>% 
  dplyr::left_join(CZ_color, by = "cluster") %>% 
  dplyr::tibble() %>% 
  dplyr::select(JISCODE, cluster, CZ_color)

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
UEA_color$UEA_color <- color_assignment
UEA_color <- UEA_color %>%
  dplyr::tibble() %>%
  select(-geometry)

UEA_2005.sf <- UEA_2005.sf %>% 
  dplyr::left_join(UEA_color, by = "UEA") %>% 
  dplyr::tibble() %>% 
  dplyr::select(JISCODE, 都市圏名, UEA_color)

base::rm(color_assignment, i, neighbors, neighbor_matrix, UEA_color)

sf_use_s2(TRUE) 

MapData <- muni_map %>% 
  dplyr::left_join(CZ_map, by = "JISCODE") %>% 
  dplyr::left_join(UEA_2005.sf, by = "JISCODE")

MapData %>%
  mapview(zcol = "CZ_color",
          label = c("NAME", "cluster", "都市圏名"),
          popup = popupTable(MapData,
                             zcol = c("NAME", "cluster", "都市圏名")),
          legend = FALSE)
