library(tidyverse)
library(sf)
library(gridExtra)

#reading data-----------------------------------------------------------------------------  

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


#n次郊外を縦につなげる---------------------------------------------------------------------

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

muni_map <- sf::read_sf("mapdata/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>% 
  tidyr::replace_na(replace = list(GNAME = ""))


UEA_2005.sf <- muni_map %>% 
  dplyr::mutate(JISCODE = dplyr::if_else(stringr::str_sub(GNAME, -1, -1) == "市", base::trunc(JISCODE * 0.01) * 100 , JISCODE)) %>% 
  dplyr::mutate(JISCODE = dplyr::if_else((stringr::str_sub(CNAME, -1, -1) == "区" & PNAME == "東京都"), base::trunc(JISCODE * 0.01) * 100 , JISCODE)) %>% 
  dplyr::mutate(JISCODE = dplyr::if_else(GNAME %in% c("川崎市", "福岡市"), JISCODE + 30, JISCODE)) %>% 
  dplyr::full_join(UEA_2005 , by = "JISCODE")

#CZmap----------------------------------------------------------------

CZ_2005 <- readr::read_csv("output/2005_original.csv") %>% 
  dplyr::rename(JISCODE = i)

CZ_map <- muni_map %>% 
  dplyr::left_join(CZ_2005, by = "JISCODE")                                                  

temp <- CZ_map %>% 
  dplyr::tibble() %>% 
  select(cluster, NAME) %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::slice_head(n = 1) %>% 
  dplyr::rename(rep = NAME)



#creating map---------------------------------------------------------

UEA_2005.sf %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_sf(aes(fill = 都市圏名), linewidth = 0.01, color = "grey") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(#ylim = c(24, 45.5),
                    #xlim = c(122.9, 149),
                    datum = NA) +
  ggplot2::labs(title = "都市雇用圏(UEA).2005")　-> UEAmap_2005

UEA_2005.sf %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_sf(aes(fill = 都市圏名)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(ylim = c(34.5, 37.1),
                    xlim = c(138, 141),
                    datum = NA) +
  ggplot2::labs(title = "関東地方・UEA(2005)")+
  theme(plot.title    = element_text(size = 10))　-> UEAmap_2005_Kanto

CZ_map %>% 
  dplyr::left_join(temp, by = "cluster") %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = rep), linewidth = 0.01, color = "grey") +
  ggplot2::theme_bw() +
  # ggplot2::scale_fill_brewer(type = "qua") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(#ylim = c(24, 45.5),
                    #xlim = c(122.9, 149),
                    datum = NA) +
  ggplot2::labs(title = "Commuting Zone(2005)") -> CZmap_2005

CZ_map %>% 
  dplyr::left_join(temp, by = "cluster") %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = rep)) +
  ggplot2::theme_bw() +
  # ggplot2::scale_fill_brewer(type = "qua") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(ylim = c(34.5, 37.1),
                    xlim = c(138, 141),
                    datum = NA) +
  ggplot2::labs(title = "関東地方・Commuting Zone(2005)")+
  theme(plot.title    = element_text(size = 10)) -> CZmap_2005_Kanto

gridExtra::grid.arrange(UEAmap_2005, CZmap_2005, nrow = 1) %>% 
  ggplot2::ggsave(filename = "output/map_image/UEA&CZmap_2005.png", bg = "white", width = 5, height = 3)
gridExtra::grid.arrange(UEAmap_2005_Kanto, CZmap_2005_Kanto, nrow = 1) %>% 
  ggplot2::ggsave(filename = "output/map_image/UEA&CZmap_2005_kanto.png", bg = "white", width = 5, height = 3)

ggsave(UEAmap_2005, filename = "output/map_image/UEAmap_2005.png", bg = "white")
ggsave(UEAmap_2005_Kanto, filename = "output/map_image/UEAmap_2005_Kanto.png", bg = "white")

ggplot2::ggsave(CZmap_2005, filename = "output/map_image/CZmap_2005.png", bg = "white")
ggplot2::ggsave(CZmap_2005_Kanto, filename = "output/map_image/CZmap_2005_Kanto.png", bg = "white")


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
