library(tidyverse)
library(sf)
library(gridExtra)
library(spdep)
library(RColorBrewer)
"%not.in%" <- Negate("%in%")



# year <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)

OkinawaLine <- base::rbind(c(138, 45), c(138, 40), c(130, 37)) %>% 
  st_linestring() %>%
  sf::st_sfc(crs = 4612) %>%
  sf::st_sf()
HokkaidoLine <- base::rbind(c(137.5, 45), c(137.5, 40), c(134, 37), c(120, 37)) %>% 
  st_linestring() %>% 
  sf::st_sfc(crs = 4612) %>% 
  sf::st_sf()
colors <- RColorBrewer::brewer.pal(8, "Set2")

path_list.McEA <- paste0("data/UEA/suburb/McEA/", list.files("data/UEA/suburb/McEA"))
path_list.McEA.C <- paste0("data/UEA/center/McEA/", list.files("data/UEA/center/McEA"))
path_list.MEA <- paste0("data/UEA/suburb/MEA/", list.files("data/UEA/suburb/MEA"))
path_list.MEA.C <- paste0("data/UEA/center/MEA/", list.files("data/UEA/center/MEA"))

assign_list <- paste0("UEA", list.files("data/UEA/suburb/McEA") %>% 
                        stringr::str_replace(pattern = "A8", replacement = "A198") %>% 
                        stringr::str_replace(pattern = "A9", replacement = "A199") %>% 
                        stringr::str_sub(start = 5, end = -5) %>% 
                        stringr::str_remove(pattern = "_Rev07"))
year <- list.files("data/UEA/suburb/McEA") %>% 
  stringr::str_replace(pattern = "A8", replacement = "A198") %>% 
  stringr::str_replace(pattern = "A9", replacement = "A199") %>% 
  stringr::str_sub(start = 5, end = -5) %>% 
  stringr::str_remove(pattern = "_Rev07")


for (i in (1:length(path_list.McEA))){
  #### data cleaning ####
  McEA <- readr::read_csv(path_list.McEA[i], locale = locale(encoding = "cp932"), show_col_types = FALSE) %>% 
    rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
    rename(UEA = 1) 
  
  McEA.C <- readr::read_csv(path_list.McEA.C[i], locale = locale(encoding = "cp932"), show_col_types = FALSE) %>% 
    rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
    rename(UEA = 1)
  
  MEA <- readr::read_csv(path_list.MEA[i], locale = locale(encoding = "cp932"), show_col_types = FALSE) %>% 
    rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
    rename(UEA = 1)
  
  MEA.C <- readr::read_csv(path_list.MEA.C[i], locale = locale(encoding = "cp932"), show_col_types = FALSE) %>% 
    rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
    rename(UEA = 1)
  
  if (ncol(McEA) == 20){
    McEA_sub4 <- McEA %>% 
      dplyr::select(UEA, 都市圏名, suburb4) %>% 
      tidyr::drop_na(suburb4) %>% 
      dplyr::rename(JISCODE = suburb4)
  }
  
  McEA_sub3 <- McEA %>% 
    dplyr::select(UEA, 都市圏名, suburb3) %>% 
    tidyr::drop_na(suburb3) %>% 
    dplyr::rename(JISCODE = suburb3)
  
  McEA_sub2 <- McEA %>% 
    dplyr::select(UEA, 都市圏名, suburb2) %>% 
    tidyr::drop_na(suburb2) %>% 
    dplyr::rename(JISCODE = suburb2)
  
  McEA_sub1 <- McEA %>% 
    dplyr::select(UEA, 都市圏名, suburb) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(JISCODE = suburb) 
  
  McEA_center <- McEA.C %>% 
    dplyr::select(UEA, 都市圏名, center) %>% 
    dplyr::rename(JISCODE = center)
  
  if (ncol(MEA) == 20){
    MEA_sub4 <- MEA %>% 
      dplyr::select(UEA, 都市圏名, suburb4) %>% 
      tidyr::drop_na(suburb4) %>% 
      dplyr::rename(JISCODE = suburb4)
  }
  
  MEA_sub3 <- MEA %>% 
    dplyr::select(UEA, 都市圏名, suburb3) %>% 
    tidyr::drop_na(suburb3) %>% 
    dplyr::rename(JISCODE = suburb3)
  
  MEA_sub2 <- MEA %>% 
    dplyr::select(UEA, 都市圏名, suburb2) %>% 
    tidyr::drop_na(suburb2) %>% 
    dplyr::rename(JISCODE = suburb2)
  
  MEA_sub1 <- MEA %>% 
    dplyr::select(UEA, 都市圏名, suburb) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(JISCODE = suburb)
  
  MEA_center <- MEA.C %>% 
    dplyr::select(UEA, 都市圏名, center) %>% 
    dplyr::rename(JISCODE = center) 
  
  if (ncol(McEA) == 20){
    UEA <- dplyr::bind_rows(McEA_center, McEA_sub1, McEA_sub2, McEA_sub3, McEA_sub4,
                            MEA_center,  MEA_sub1,  MEA_sub2,  MEA_sub3,  MEA_sub4) %>% 
      tidyr::drop_na(JISCODE)
  } else {
    UEA <- dplyr::bind_rows(McEA_center, McEA_sub1, McEA_sub2, McEA_sub3,
                            MEA_center,  MEA_sub1,  MEA_sub2,  MEA_sub3) %>% 
      tidyr::drop_na(JISCODE)
  }
  # assign(assign_list[i], UEA)
  
  #### map making ####
  if(year[i] != 2015){
    codePath <- paste0("mapdata/codelist_", year[i], "1001and20151001.csv")
    code <- readr::read_csv(codePath, locale = locale(encoding = "cp932")) %>%
      dplyr::mutate(JISCODE = as.numeric(JISCODE1),
                    JISCODE_2015 = as.numeric(JISCODE2)) %>% 
      dplyr::select(JISCODE, JISCODE_2015)
    UEA <- UEA %>% 
      dplyr::left_join(code, by = "JISCODE") %>% 
      dplyr::select(-JISCODE) %>% 
      dplyr::rename(JISCODE = JISCODE_2015) %>% 
      dplyr::distinct()
  }
  czPath <- paste0("output/", year[i], "_harmonized.csv")
  
  muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>% 
    dplyr::filter(JISCODE != 13421,
                  JISCODE %not.in% c(1695, 1696, 1698)) %>% 
    # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
    dplyr::select(-NO, -DATE) %>% 
    sf::st_transform(4612)
  
  UEA.sf <- muni.sf %>% 
    dplyr::mutate(JISCODE = dplyr::if_else((stringr::str_sub(CNAME, -1, -1) == "区" & PNAME == "東京都"), 
                                           base::trunc(JISCODE * 0.01) * 100 , 
                                           JISCODE)) %>% 
    dplyr::left_join(UEA, by = "JISCODE")
  
  CZ.sf <- muni.sf %>% 
    dplyr::left_join(readr::read_csv(czPath), by = c("JISCODE" = "i"))
  
  sf_use_s2(FALSE) 
  
  UEA_color <- UEA.sf %>%
    tidyr::drop_na(UEA) %>% 
    dplyr::group_by(UEA) %>%
    dplyr::select(UEA) %>%
    dplyr::summarise() %>%
    sf::st_make_valid()
  
  neighbors <- spdep::poly2nb(UEA_color)
  neighbor_matrix <- spdep::nb2mat(neighbors, style = "B", zero.policy = TRUE)
  
  color_assignment <- rep(NA, length(neighbors))
  color_assignment[which(UEA_color$UEA == 13100)] <- colors[1]
  roop <- (1:length(neighbors))[-which(UEA_color$UEA == 13100)]
  
  for (j in roop) {
    available_colors <- setdiff(colors, color_assignment[neighbors[[j]]])
    color_assignment[j] <- available_colors[1]
  }
  UEA_color$color <- color_assignment
  UEA_color <- UEA_color %>%
    dplyr::tibble() %>%
    select(-geometry)
  
  UEA.sf <- dplyr::left_join(UEA.sf, UEA_color, by = "UEA")
  
  rm(j, neighbors, neighbor_matrix, color_assignment, available_colors, UEA_color)
  
  CZ_color <- CZ.sf %>%
    dplyr::group_by(cluster) %>%
    dplyr::select(cluster) %>%
    dplyr::summarise() %>%
    sf::st_make_valid()
  
  neighbors <- spdep::poly2nb(CZ_color)
  neighbor_matrix <- spdep::nb2mat(neighbors, style = "B", zero.policy = TRUE)
  
  
  color_assignment <- rep(NA, length(neighbors))
  
  for (j in 1:length(neighbors)) {
    available_colors <- setdiff(colors, color_assignment[neighbors[[j]]])
    color_assignment[j] <- available_colors[1]
  }
  CZ_color$color <- color_assignment
  CZ_color <- CZ_color %>%
    dplyr::tibble() %>%
    select(-geometry)
  
  CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster")
  
  rm(j, neighbors, neighbor_matrix, color_assignment, available_colors, CZ_color)
  
  movement_okinwa <- UEA.sf %>%
    dplyr::filter(JISCODE %in% (47000:47999)) %>% 
    sf::st_set_geometry(sf::st_geometry(UEA.sf %>% dplyr::filter(JISCODE %in% (47000:47999))) + c(5, 15)) %>% 
    sf::st_set_crs(4612)
  
  UEA.sf.whole <- UEA.sf %>% 
    dplyr::filter(JISCODE %not.in% (47000:47999)) %>% 
    dplyr::bind_rows(movement_okinwa) 
  
  movement_okinwa <- CZ.sf %>%
    dplyr::filter(JISCODE %in% (47000:47999)) %>% 
    sf::st_set_geometry(sf::st_geometry(CZ.sf %>% dplyr::filter(JISCODE %in% (47000:47999))) + c(5, 15)) %>% 
    sf::st_set_crs(4612)
  
  CZ.sf.whole <- CZ.sf %>% 
    dplyr::filter(JISCODE %not.in% (47000:47999)) %>% 
    dplyr::bind_rows(movement_okinwa) 
  
  movement_Hokkaido <- UEA.sf %>% 
    dplyr::filter(JISCODE %in% (1000:1999)) %>% 
    sf::st_set_geometry(st_geometry(UEA.sf %>% dplyr::filter(JISCODE %in% (1000:1999))) - c(10, 4)) %>% 
    sf::st_set_crs(4612)
  
  UEA.sf.enlarged <- UEA.sf %>% 
    dplyr::filter(JISCODE %not.in% (1000:1999)) %>% 
    dplyr::bind_rows(movement_Hokkaido) 
  
  movement_Hokkaido <- CZ.sf %>% 
    dplyr::filter(JISCODE %in% (1000:1999)) %>% 
    sf::st_set_geometry(st_geometry(CZ.sf %>% dplyr::filter(JISCODE %in% (1000:1999))) - c(10, 4)) %>% 
    sf::st_set_crs(4612)
  
  CZ.sf.enlarged <- CZ.sf %>% 
    dplyr::filter(JISCODE %not.in% (1000:1999)) %>% 
    dplyr::bind_rows(movement_Hokkaido) 
  
  rm(movement_okinwa, movement_Hokkaido)
  
  UEA.sf.whole %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = OkinawaLine) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = paste("UEA", year[i])) +
    ggplot2::theme(plot.title = element_text(size = 5)) -> UEAmap.whole
  
  CZ.sf.whole %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = OkinawaLine) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = paste("CZ", year[i])) +
    ggplot2::theme(plot.title = element_text(size = 5)) -> CZmap.whole
  
  gridExtra::grid.arrange(UEAmap.whole, CZmap.whole, nrow = 1) %>% 
    ggplot2::ggsave(filename = paste0("output/map_image/TimeSeries_UEAandCZ/harmonized/Whole/", year[i], "_UEAandCZmap.png"), 
                    bg = "white", width = 5, height = 3)
  
  
  UEA.sf.enlarged %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = HokkaidoLine) +
    ggplot2::coord_sf(ylim = c(31.2, 42),
                      xlim = c(129.3, 142.3),
                      datum = NA) +
    ggplot2::labs(title = paste("UEA", year[i]))+
    ggplot2::theme(plot.title = element_text(size = 5)) -> UEAmap.enlaged
  
  CZ.sf.enlarged %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = HokkaidoLine) +
    ggplot2::coord_sf(ylim = c(31.2, 42),
                      xlim = c(129.3, 142.3),
                      datum = NA) +
    ggplot2::labs(title = paste("CZ", year[i]))+
    ggplot2::theme(plot.title = element_text(size = 5)) -> CZmap.enlaged
  
  gridExtra::grid.arrange(UEAmap.enlaged, CZmap.enlaged, nrow = 1) %>% 
    ggplot2::ggsave(filename = paste0("output/map_image/TimeSeries_UEAandCZ/harmonized/enlaged_MainLands/", year[i], "_UEAandCZmap.png"), 
                    bg = "white", width = 5, height = 3)
  
  
  UEA.sf %>% 
    dplyr::filter(JISCODE %not.in% (25000:47999),
                  JISCODE %not.in% (1000:5999)) %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.5, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = paste("関東地方 UEA", year[i]))+
    theme(plot.title    = element_text(size = 5))　-> UEAmap.Kanto
  
  CZ.sf %>% 
    dplyr::filter(JISCODE %not.in% (25000:47999),
                  JISCODE %not.in% (1000:5999)) %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.5, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = paste("関東地方 CZ", year[i]))+
    theme(plot.title    = element_text(size = 5))　-> CZmap.Kanto
  
  gridExtra::grid.arrange(UEAmap.Kanto, CZmap.Kanto, nrow = 1) %>% 
    ggplot2::ggsave(filename = paste0("output/map_image/TimeSeries_UEAandCZ/harmonized/Kanto/", year[i], "_UEAandCZmap.png"), 
                    bg = "white", width = 5, height = 3)
  
  
  fileName.whole <- paste0("output/map_image/TimeSeries_UEA/harmonized/Whole/", year[i], "_UEAmap.png")
  fileName.enlaged <- paste0("output/map_image/TimeSeries_UEA/harmonized/Enlaged_MainLands/", year[i], "_UEAmap.png")
  fileName.kanto <- paste0("output/map_image/TimeSeries_UEA/harmonized/Enlaged_Kanto/", year[i], "_UEAmap_Kanto.png")
  
  ggsave(UEAmap.whole, filename = fileName.whole, bg = "white")
  ggsave(UEAmap.enlaged, filename = fileName.enlaged, bg = "white")
  ggsave(UEAmap.Kanto, filename = fileName.kanto, bg = "white")
  
  assign(paste0(assign_list[i], "_whole"), UEAmap.whole)
  assign(paste0(assign_list[i], "_enlarged"), UEAmap.enlaged)
  assign(paste0(assign_list[i], "_kanto"), UEAmap.Kanto)
}

gridExtra::grid.arrange(UEA1980_whole, UEA1990_whole, UEA1995_whole, UEA2000_whole,
                        UEA2005_whole, UEA2010_whole, UEA2015_whole, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/TimeSeries_UEA/harmonized/multiple/1980to2015_UEAmap_Whole.png", bg = "white")

gridExtra::grid.arrange(UEA1980_enlarged, UEA1990_enlarged, UEA1995_enlarged, UEA2000_enlarged,
                        UEA2005_enlarged, UEA2010_enlarged, UEA2015_enlarged, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/TimeSeries_UEA/harmonized/multiple/1980to2015_UEAmap_enlarged.png", bg = "white")

gridExtra::grid.arrange(UEA1980_kanto, UEA1990_kanto, UEA1995_kanto, UEA2000_kanto,
                        UEA2005_kanto, UEA2010_kanto, UEA2015_kanto, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/TimeSeries_UEA/harmonized/multiple/1980to2015_UEAmap_kanto.png", bg = "white")


library(beepr)
beepr::beep()
