library(tidyverse)
library(sf)
library(gridExtra)
library(spdep)
library(RColorBrewer)
"%not.in%" <- Negate("%in%")



# year <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
# 
lineMatrix = base::rbind(c(138, 45), c(138, 40), c(130, 37))
OkinawaLine <- st_linestring(lineMatrix) %>%
  sf::st_sfc() %>%
  sf::st_set_crs(4612)
lineMatrix = base::rbind(c(139.5, 41), c(137.5, 40), c(137.5, 38), c(134, 37), c(130, 37))
HokkaidoLine <- st_linestring(lineMatrix) %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4612)
rm(lineMatrix)
colors <- RColorBrewer::brewer.pal(8, "Set2")


path_list.McEA <- paste0("data/UEA/suburb/McEA/", list.files("data/UEA/suburb/McEA"))
path_list.McEA.C <- paste0("data/UEA/center/McEA/", list.files("data/UEA/center/McEA"))
path_list.MEA <- paste0("data/UEA/suburb/MEA/", list.files("data/UEA/suburb/MEA"))
path_list.MEA.C <- paste0("data/UEA/center/MEA/", list.files("data/UEA/center/MEA"))

assign_list <- paste0("UEA", list.files("data/UEA/suburb/McEA") %>% 
                        stringr::str_replace(pattern = "A8", replacement = "A198") %>% 
                        stringr::str_replace(pattern = "A9", replacement = "A199") %>% 
                        stringr::str_sub(start = 5, end = -5) %>% 
                        stringr::str_remove(pattern = "_Rev07")
)

year <- list.files("data/UEA/suburb/McEA") %>% 
  stringr::str_replace(pattern = "A8", replacement = "A198") %>% 
  stringr::str_replace(pattern = "A9", replacement = "A199") %>% 
  stringr::str_sub(start = 5, end = -5) %>% 
  stringr::str_remove(pattern = "_Rev07")

Rail.row <- sf::read_sf("data/N05-23_GML/N05-23_RailroadSection2.shp") %>% 
  dplyr::rename(ID = N05_006) %>% 
  dplyr::mutate(constructed = as.integer(N05_004),
                start = as.integer(N05_005b),
                end = as.integer(N05_005e)) %>% 
  dplyr::select(ID, constructed, start, end) %>% 
  sf::st_transform(4612)

UEA_map <- list()
UEA_map.kanto <- list()
CZ_map <- list()
CZ_map.kanto <- list()

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
      dplyr::select(UEA, 都市圏名, suburb4, suburb3) %>% 
      tidyr::drop_na(suburb4) %>% 
      dplyr::rename(JISCODE = suburb4,
                    depend_sub = suburb3) %>% 
      dplyr::mutate(order_suburb = "4")
  }
  
  McEA_sub3 <- McEA %>% 
    dplyr::select(UEA, 都市圏名, suburb3, suburb2) %>% 
    tidyr::drop_na(suburb3) %>% 
    dplyr::rename(JISCODE = suburb3,
                  depend_sub = suburb2) %>% 
    dplyr::mutate(order_suburb = "3")
  
  McEA_sub2 <- McEA %>% 
    dplyr::select(UEA, 都市圏名, suburb2, suburb) %>% 
    tidyr::drop_na(suburb2) %>% 
    dplyr::rename(JISCODE = suburb2,
                  depend_sub = suburb) %>% 
    dplyr::mutate(order_suburb = "2")
  
  McEA_sub1 <- McEA %>% 
    dplyr::select(UEA, 都市圏名, suburb) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(JISCODE = suburb) %>% 
    dplyr::mutate(order_suburb = "1")
  
  McEA_center <- McEA.C %>% 
    dplyr::select(UEA, 都市圏名, center) %>% 
    dplyr::rename(JISCODE = center) %>% 
    dplyr::mutate(order_suburb = "0")
  
  if (ncol(MEA) == 20){
    MEA_sub4 <- MEA %>% 
      dplyr::select(UEA, 都市圏名, suburb4, suburb3) %>% 
      tidyr::drop_na(suburb4) %>% 
      dplyr::rename(JISCODE = suburb4,
                    depend_sub = suburb3) %>% 
      dplyr::mutate(order_suburb = "4")
  }
  
  MEA_sub3 <- MEA %>% 
    dplyr::select(UEA, 都市圏名, suburb3, suburb2) %>% 
    tidyr::drop_na(suburb3) %>% 
    dplyr::rename(JISCODE = suburb3,
                  depend_sub = suburb2) %>% 
    dplyr::mutate(order_suburb = "3")
  
  MEA_sub2 <- MEA %>% 
    dplyr::select(UEA, 都市圏名, suburb2, suburb) %>% 
    tidyr::drop_na(suburb2) %>% 
    dplyr::rename(JISCODE = suburb2,
                  depend_sub = suburb) %>% 
    dplyr::mutate(order_suburb = "2")
  
  MEA_sub1 <- MEA %>% 
    dplyr::select(UEA, 都市圏名, suburb) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(JISCODE = suburb) %>% 
    dplyr::mutate(order_suburb = "1")
  
  MEA_center <- MEA.C %>% 
    dplyr::select(UEA, 都市圏名, center) %>% 
    dplyr::rename(JISCODE = center) %>% 
    dplyr::mutate(order_suburb = "0")
  
  if (ncol(McEA) == 20){
    UEA <- dplyr::bind_rows(McEA_center, McEA_sub1, McEA_sub2, McEA_sub3, McEA_sub4,
                            MEA_center,  MEA_sub1,  MEA_sub2,  MEA_sub3,  MEA_sub4) %>% 
      tidyr::drop_na(JISCODE)
  } else {
    UEA <- dplyr::bind_rows(McEA_center, McEA_sub1, McEA_sub2, McEA_sub3,
                            MEA_center,  MEA_sub1,  MEA_sub2,  MEA_sub3) %>% 
      tidyr::drop_na(JISCODE)
  }
  
  rm(MEA_center, MEA_sub1, MEA_sub2, MEA_sub3 ,MEA_sub4,
     McEA_center, McEA_sub1, McEA_sub2, McEA_sub3, McEA_sub4,
     MEA.C, MEA, McEA.C, McEA)
  # assign(assign_list[i], UEA)
  
  #### map making ####
  
  mapPath <- paste0("mapdata/mmm", year[i], "1001/mmm", year[i], "1001.shp")
  czPath <- paste0("output/", year[i], "_original.csv")
  
  muni.sf <- sf::read_sf(mapPath, options = "ENCODING=CP932") %>% 
    dplyr::filter(JISCODE != 13421,
                  JISCODE %not.in% c(1695, 1696, 1698)) %>% # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
    dplyr::select(-NO, -DATE) %>% 
    sf::st_transform(4612)
  
  UEA.sf <- muni.sf %>% 
    dplyr::mutate(JISCODE = dplyr::if_else((stringr::str_sub(CNAME, -1, -1) == "区" & PNAME == "東京都"), 
                                           base::trunc(JISCODE * 0.01) * 100 , 
                                           JISCODE
    )
    ) %>% 
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
  
  #### 北海道･沖縄 #############################################################
  
  # movement_okinwa <- UEA.sf %>%
  #   dplyr::filter(JISCODE %in% (47000:47999)) %>% 
  #   sf::st_set_geometry(sf::st_geometry(UEA.sf %>% dplyr::filter(JISCODE %in% (47000:47999))) + c(5, 15)) %>% 
  #   sf::st_set_crs(4612)
  # 
  # UEA.sf.whole <- UEA.sf %>% 
  #   dplyr::filter(JISCODE %not.in% (47000:47999)) %>% 
  #   dplyr::bind_rows(movement_okinwa) 
  # 
  # movement_okinwa <- CZ.sf %>%
  #   dplyr::filter(JISCODE %in% (47000:47999)) %>% 
  #   sf::st_set_geometry(sf::st_geometry(CZ.sf %>% dplyr::filter(JISCODE %in% (47000:47999))) + c(5, 15)) %>% 
  #   sf::st_set_crs(4612)
  # 
  # CZ.sf.whole <- CZ.sf %>% 
  #   dplyr::filter(JISCODE %not.in% (47000:47999)) %>% 
  #   dplyr::bind_rows(movement_okinwa) 
  # 
  # movement_Hokkaido <- UEA.sf %>% 
  #   dplyr::filter(JISCODE %in% (1000:1999)) %>% 
  #   sf::st_set_geometry(st_geometry(UEA.sf %>% dplyr::filter(JISCODE %in% (1000:1999))) - c(10, 4)) %>% 
  #   sf::st_set_crs(4612)
  # 
  # UEA.sf.enlarged <- UEA.sf %>% 
  #   dplyr::filter(JISCODE %not.in% (1000:1999)) %>% 
  #   dplyr::bind_rows(movement_Hokkaido) 
  # 
  # movement_Hokkaido <- CZ.sf %>% 
  #   dplyr::filter(JISCODE %in% (1000:1999)) %>% 
  #   sf::st_set_geometry(st_geometry(CZ.sf %>% dplyr::filter(JISCODE %in% (1000:1999))) - c(10, 4)) %>% 
  #   sf::st_set_crs(4612)
  # 
  # CZ.sf.enlarged <- CZ.sf %>% 
  #   dplyr::filter(JISCODE %not.in% (1000:1999)) %>% 
  #   dplyr::bind_rows(movement_Hokkaido) 
  # 
  # rm(movement_okinwa, movement_Hokkaido)
  
  
  
  ### Rail #####################################################################
  Yr <- year[i] %>% as.numeric()
  Rail <- Rail.row %>% 
    dplyr::filter(end >= Yr,
                  start <= Yr)
  
  ### Whole ####################################################################
  # UEA.sf.whole %>% 
  #   ggplot2::ggplot() + 
  #   ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
  #   ggplot2::scale_fill_manual(values = colors) +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(legend.position = "none") +
  #   ggplot2::geom_sf(data = OkinawaLine) +
  #   ggplot2::coord_sf(datum = NA) +
  #   ggplot2::labs(title = paste("UEA", year[i])) +
  #   ggplot2::theme(plot.title = element_text(size = 5)) -> UEAmap.whole
  # 
  # CZ.sf.whole %>% 
  #   ggplot2::ggplot() + 
  #   ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
  #   ggplot2::scale_fill_manual(values = colors) +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(legend.position = "none") +
  #   ggplot2::geom_sf(data = OkinawaLine) +
  #   ggplot2::coord_sf(datum = NA) +
  #   ggplot2::labs(title = paste("CZ", year[i])) +
  #   ggplot2::theme(plot.title = element_text(size = 5)) -> CZmap.whole
  # 
  # gridExtra::grid.arrange(UEAmap.whole, CZmap.whole, nrow = 1) %>% 
  #   ggplot2::ggsave(filename = paste0("output/map_image/TimeSeries_UEAandCZ/Original/Whole/", year[i], "_UEAandCZmap.png"), 
  #                   bg = "white", width = 5, height = 3)
  # 
  ### Enlaged main lands ####################################################### 
  # UEA.sf.enlarged %>% 
  #   ggplot2::ggplot() + 
  #   ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
  #   ggplot2::scale_fill_manual(values = colors) +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(legend.position = "none") +
  #   ggplot2::geom_sf(data = HokkaidoLine) +
  #   ggplot2::coord_sf(ylim = c(31.2, 42),
  #                     xlim = c(129.3, 142.3),
  #                     datum = NA) +
  #   ggplot2::labs(title = paste("UEA", year[i]))+
  #   ggplot2::theme(plot.title = element_text(size = 5)) -> UEAmap.enlaged
  # 
  # CZ.sf.enlarged %>% 
  #   ggplot2::ggplot() + 
  #   ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
  #   ggplot2::scale_fill_manual(values = colors) +
  #   ggplot2::theme_bw() +
  #   ggplot2::theme(legend.position = "none") +
  #   ggplot2::geom_sf(data = HokkaidoLine) +
  #   ggplot2::coord_sf(ylim = c(31.2, 42),
  #                     xlim = c(129.3, 142.3),
  #                     datum = NA) +
  #   ggplot2::labs(title = paste("CZ", year[i]))+
  #   ggplot2::theme(plot.title = element_text(size = 5)) -> CZmap.enlaged
  # 
  # gridExtra::grid.arrange(UEAmap.enlaged, CZmap.enlaged, nrow = 1) %>% 
  #   ggplot2::ggsave(filename = paste0("output/map_image/TimeSeries_UEAandCZ/Original/enlaged_MainLands/", year[i], "_UEAandCZmap.png"), 
  #                   bg = "white", width = 5, height = 3)
  # 
  ### Kanto ####################################################################
  UEA.sf %>%
    dplyr::filter(JISCODE %not.in% (25000:47999),
                  JISCODE %not.in% (1000:5999)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .1, alpha = .5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.5, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = paste("関東地方 UEA", year[i]))+
    theme(plot.title    = element_text(size = 5))　-> UEAmap.Kanto
  
  fileName.kanto.UEA <- paste0("output/map_image/Railroad/Original/Kanto/UEA/", year[i], "_UEAmap_Kanto.png")
  ggsave(UEAmap.Kanto, filename = fileName.kanto.UEA, bg = "white")
  UEA_map.kanto <- append(UEA_map.kanto, list(UEAmap.Kanto))

  CZ.sf %>%
    dplyr::filter(JISCODE %not.in% (25000:47999),
                  JISCODE %not.in% (1000:5999)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .1, alpha = .5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.6, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = paste("関東地方 CZ", year[i]))+
    theme(plot.title    = element_text(size = 5))　-> CZmap.Kanto
  fileName.kanto.CZ <- paste0("output/map_image/Railroad/Original/Kanto/CZ/", year[i], "_CZmap_Kanto.png")
  ggsave(UEAmap.Kanto, filename = fileName.kanto.CZ, bg = "white")
  CZ_map.kanto <- append(CZ_map.kanto, list(CZmap.Kanto))
  
  gridExtra::grid.arrange(UEAmap.Kanto, CZmap.Kanto, nrow = 1) %>%
    ggplot2::ggsave(filename = paste0("output/map_image/Railroad/Original/Kanto/double/", year[i], "_UEAandCZmap.png"),
                    bg = "white", width = 5, height = 3)
  
  
  ### no move ##################################################################
  CZ.sf %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = 0.1, alpha = .5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    # ggplot2::geom_sf(data = HokkaidoLine) +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = paste("CZ", year[i])) -> CZmap_rail
  
  fileName.CZ <- paste0("output/map_image/Railroad/Original/Whole/CZ/", year[i], "_CZmap_Kanto.png")
  ggsave(CZmap_rail, filename = fileName.CZ, bg = "white")
  CZ_map <- append(CZ_map, list(CZmap_rail))
  
  
  UEA.sf %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = 0.1, alpha = .5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    # ggplot2::geom_sf(data = HokkaidoLine) +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = paste("UEA", year[i])) -> UEAmap_rail
  
  fileName.UEA <- paste0("output/map_image/Railroad/Original/Whole/UEA/", year[i], "_UEAmap_Kanto.png")
  ggsave(CZmap_rail, filename = fileName.UEA, bg = "white")
  UEA_map <- append(UEA_map, list(UEAmap_rail))
  
  gridExtra::grid.arrange(CZmap_rail, UEAmap_rail, nrow = 1) %>%
    ggplot2::ggsave(filename = paste0("output/map_image/Railroad/Original/Whole/double/", year[i], "_UEAandCZmap.png"),
                    bg = "white", width = 5, height = 3)
  if (i == 1) {
    muni.sf <- sf::read_sf("mapdata/mmm19851001/mmm19851001.shp", options = "ENCODING=CP932") %>% 
      dplyr::filter(JISCODE != 13421,
                    JISCODE %not.in% c(1695, 1696, 1698)) %>% # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
      dplyr::select(-NO, -DATE) %>% 
      sf::st_transform(4612)
    
    
    CZ.sf <- muni.sf %>% 
      dplyr::left_join(readr::read_csv("output/1985_original.csv"), by = c("JISCODE" = "i"))
    
    sf_use_s2(FALSE) 
    
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
    Rail <- Rail.row %>% 
      dplyr::filter(end >= 1985,
                    start <= 1985)
    CZ.sf %>%
      dplyr::filter(JISCODE %not.in% (25000:47999),
                    JISCODE %not.in% (1000:5999)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = color)) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2, alpha = .8) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_sf(ylim = c(34.6, 37.1),
                        xlim = c(138, 141),
                        datum = NA) +
      ggplot2::labs(title = "関東地方 CZ 1985")+
      theme(plot.title    = element_text(size = 5))　-> CZmap.Kanto
    ggsave(UEAmap.Kanto, filename = "output/map_image/Railroad/Original/Kanto/CZ/1985_CZmap_Kanto.png", bg = "white")
    CZ_map.kanto <- append(CZ_map.kanto, list(CZmap.Kanto))
    
    CZ.sf %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::geom_sf(data = Rail, color = "black", linewidth = 0.1, alpha = .8) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      # ggplot2::geom_sf(data = HokkaidoLine) +
      ggplot2::coord_sf(ylim = c(31.2, 45.5),
                        xlim = c(129.3, 145.8),
                        datum = NA) +
      ggplot2::labs(title = "CZ 1985") -> CZmap_rail
    
    ggsave(CZmap_rail, filename = "output/map_image/Railroad/Original/Whole/CZ/1985_CZmap_Kanto.png", bg = "white")
    CZ_map <- append(CZ_map, list(CZmap_rail))
  }
  
  
  # fileName.whole <- paste0("output/map_image/TimeSeries_UEA/Original/Whole/", year[i], "_UEAmap.png")
  # fileName.enlaged <- paste0("output/map_image/TimeSeries_UEA/Original/Enlaged_MainLands/", year[i], "_UEAmap.png")
  
  # ggsave(UEAmap.whole, filename = fileName.whole, bg = "white")
  # ggsave(UEAmap.enlaged, filename = fileName.enlaged, bg = "white")

  # assign(paste0(assign_list[i], "_whole"), UEAmap.whole)
  # assign(paste0(assign_list[i], "_enlarged"), UEAmap.enlaged)
}

# gridExtra::grid.arrange(UEA1980_whole, UEA1990_whole, UEA1995_whole, UEA2000_whole,
#                         UEA2005_whole, UEA2010_whole, UEA2015_whole, nrow = 3) %>%
#   ggplot2::ggsave(filename = "output/map_image/TimeSeries_UEA/Original/multiple/1980to2015_UEAmap_Whole.png", bg = "white")
# 
# gridExtra::grid.arrange(UEA1980_enlarged, UEA1990_enlarged, UEA1995_enlarged, UEA2000_enlarged,
#                         UEA2005_enlarged, UEA2010_enlarged, UEA2015_enlarged, nrow = 3) %>%
#   ggplot2::ggsave(filename = "output/map_image/TimeSeries_UEA/Original/multiple/1980to2015_UEAmap_enlarged.png", bg = "white")

gridExtra::grid.arrange(grobs = UEA_map.kanto, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/Railroad/Original/multiple/1980to2015_UEAmap_kanto.png", bg = "white")
gridExtra::grid.arrange(grobs = CZ_map.kanto, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/Railroad/Original/multiple/1980to2015_CZmap_kanto.png", bg = "white")
gridExtra::grid.arrange(grobs = UEA_map, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/Railroad/Original/multiple/1980to2015_UEAmap.png", bg = "white")
gridExtra::grid.arrange(grobs = CZ_map, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/Railroad/Original/multiple/1980to2015_CZmap.png", bg = "white")

library(beepr)
beepr::beep()

