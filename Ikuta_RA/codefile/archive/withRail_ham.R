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
colors <- RColorBrewer::brewer.pal(7, "Set2")


path_list.McEA <- paste0("data/UEA/suburb/McEA/", list.files("data/UEA/suburb/McEA"))
path_list.McEA.C <- paste0("data/UEA/center/McEA/", list.files("data/UEA/center/McEA"))
path_list.MEA <- paste0("data/UEA/suburb/MEA/", list.files("data/UEA/suburb/MEA"))
path_list.MEA.C <- paste0("data/UEA/center/MEA/", list.files("data/UEA/center/MEA"))


year <- list.files("data/UEA/suburb/McEA") %>% 
  stringr::str_replace(pattern = "A8", replacement = "A198") %>% 
  stringr::str_replace(pattern = "A9", replacement = "A199") %>% 
  stringr::str_sub(start = 5, end = -5) %>% 
  stringr::str_remove(pattern = "_Rev07")

Rail.row <- sf::read_sf("data/N05-23_GML/N05-23_RailroadSection2.shp")

SHR.row <- Rail.row %>% 
  dplyr::filter(stringr::str_detect(N05_002, "新幹線")) %>% 
  dplyr::rename(ID = N05_006) %>% 
  dplyr::mutate(constructed = as.integer(N05_004),
                start = as.integer(N05_005b),
                end = as.integer(N05_005e)) %>% 
  dplyr::select(ID, constructed, start, end) %>% 
  sf::st_transform(4612)

Rail.row <- Rail.row %>% 
  dplyr::filter(stringr::str_detect(N05_002, "新幹線", negate = TRUE)) %>% 
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

muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>% 
  dplyr::filter(JISCODE != 13421,
                JISCODE %not.in% c(1695, 1696, 1698)) %>% # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
  dplyr::select(-NO, -DATE) %>% 
  sf::st_transform(4612)

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
  
  UEA.sf <- dplyr::left_join(UEA.sf, UEA_color, by = "UEA") %>% 
    dplyr::select(JISCODE, color)
  
  rm(j, neighbors, color_assignment, available_colors, UEA_color)
  
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
  CZ_color <- CZ_color %>%
    dplyr::tibble() %>%
    select(-geometry)
  
  CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>% 
    dplyr::select(JISCODE, color)
  
  rm(j, neighbors, color_assignment, available_colors, CZ_color, EdoCenter, Gohunai)
  
  ### Rail #####################################################################
  Yr <- year[i] %>% as.numeric()
  Rail <- Rail.row %>% 
    dplyr::filter(end >= Yr,
                  start <= Yr)
  
  SHR <- SHR.row %>% 
    dplyr::filter(end >= Yr,
                  start <= Yr)
  
  
  ### Kanto ####################################################################
  UEA.sf %>%
    dplyr::filter(JISCODE %not.in% (25000:47999),
                  JISCODE %not.in% (1000:5999)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(fill = "transparent",color = "grey", linewidth = .02) +
    ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0, alpha = .6) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.6, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = paste("関東地方 UEA", year[i]))+
    theme(plot.title    = element_text(size = 5))　-> UEAmap.Kanto
  
  fileName.kanto.UEA <- paste0("output/map_image/Railroad/Harmonized/Kanto/UEA/", year[i], "_UEAmap_Kanto.png")
  ggsave(UEAmap.Kanto, filename = fileName.kanto.UEA, bg = "white")
  UEA_map.kanto <- append(UEA_map.kanto, list(UEAmap.Kanto))
  
  CZ.sf %>%
    dplyr::filter(JISCODE %not.in% (25000:47999),
                  JISCODE %not.in% (1000:5999)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(fill = "transparent",color = "grey", linewidth = .02) +
    ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0, alpha = .6) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.6, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = paste("関東地方 CZ", year[i]))+
    theme(plot.title    = element_text(size = 5))　-> CZmap.Kanto
  fileName.kanto.CZ <- paste0("output/map_image/Railroad/Harmonized/Kanto/CZ/", year[i], "_CZmap_Kanto.png")
  ggsave(UEAmap.Kanto, filename = fileName.kanto.CZ, bg = "white")
  CZ_map.kanto <- append(CZ_map.kanto, list(CZmap.Kanto))
  
  gridExtra::grid.arrange(UEAmap.Kanto, CZmap.Kanto, nrow = 1) %>%
    ggplot2::ggsave(filename = paste0("output/map_image/Railroad/Harmonized/Kanto/double/", year[i], "_UEAandCZmap.png"),
                    bg = "white", width = 5, height = 3)
  
  
  ### no move ##################################################################
  CZ.sf %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(fill = "transparent",color = "grey", linewidth = .01) +
    ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .1) +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0, alpha = .6) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    # ggplot2::geom_sf(data = HokkaidoLine) +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = paste("CZ", year[i])) +
    theme(plot.title    = element_text(size = 5))-> CZmap_rail
  
  fileName.CZ <- paste0("output/map_image/Railroad/Harmonized/Whole/CZ/", year[i], "_CZmap_Kanto.png")
  ggsave(CZmap_rail, filename = fileName.CZ, bg = "white")
  CZ_map <- append(CZ_map, list(CZmap_rail))
  
  
  UEA.sf %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(fill = "transparent",color = "grey", linewidth = .01) +
    ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .1) +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0, alpha = .6) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    # ggplot2::geom_sf(data = HokkaidoLine) +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = paste("UEA", year[i])) +
    theme(plot.title    = element_text(size = 5)) -> UEAmap_rail
  
  fileName.UEA <- paste0("output/map_image/Railroad/Harmonized/Whole/UEA/", year[i], "_UEAmap_Kanto.png")
  ggsave(CZmap_rail, filename = fileName.UEA, bg = "white")
  UEA_map <- append(UEA_map, list(UEAmap_rail))
  
  gridExtra::grid.arrange(CZmap_rail, UEAmap_rail, nrow = 1) %>%
    ggplot2::ggsave(filename = paste0("output/map_image/Railroad/Harmonized/Whole/double/", year[i], "_UEAandCZmap.png"),
                    bg = "white", width = 5, height = 3)
  # 1985年だけUEAがかけてるのでCZだけ作る
  if (i == 5) {
    CZ.sf <- muni.sf %>% 
      dplyr::left_join(readr::read_csv("output/1985_harmonized.csv"), by = c("JISCODE" = "i"))
    
    sf_use_s2(FALSE) 
    
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
    CZ_color <- CZ_color %>%
      dplyr::tibble() %>%
      select(-geometry)
    
    CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>% 
      dplyr::select(JISCODE, color)
    
    rm(j, neighbors, neighbor_matrix, color_assignment, available_colors, CZ_color)
    Rail <- Rail.row %>% 
      dplyr::filter(end >= 1985,
                    start <= 1985)
    SHR <- SHR.row %>% 
      dplyr::filter(end >= 1985,
                    start <= 1985)
    
    CZ.sf %>%
      dplyr::filter(JISCODE %not.in% (25000:47999),
                    JISCODE %not.in% (1000:5999)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(fill = "transparent",color = "grey", linewidth = .02) +
      ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
      ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
      ggplot2::geom_sf(aes(fill = color), linewidth = 0, alpha = .6) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_sf(ylim = c(34.6, 37.1),
                        xlim = c(138, 141),
                        datum = NA) +
      ggplot2::labs(title = "関東地方 CZ 1985")+
      theme(plot.title    = element_text(size = 5))　-> CZmap.Kanto
    ggsave(UEAmap.Kanto, filename = "output/map_image/Railroad/Harmonized/Kanto/CZ/1985_CZmap_Kanto.png", bg = "white")
    CZ_map.kanto <- append(CZ_map.kanto, list(CZmap.Kanto))
    
    CZ.sf %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(fill = "transparent",color = "grey", linewidth = .02) +
      ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .1, linetype = "dashed") +
      ggplot2::geom_sf(data = Rail, color = "black", linewidth = .1) +
      ggplot2::geom_sf(aes(fill = color), linewidth = 0, alpha = .6) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      # ggplot2::geom_sf(data = HokkaidoLine) +
      ggplot2::coord_sf(ylim = c(31.2, 45.5),
                        xlim = c(129.3, 145.8),
                        datum = NA) +
      ggplot2::labs(title = "CZ 1985") +
      theme(plot.title    = element_text(size = 5))-> CZmap_rail
    
    ggsave(CZmap_rail, filename = "output/map_image/Railroad/Harmonized/Whole/CZ/1985_CZmap_Kanto.png", bg = "white")
    CZ_map <- append(CZ_map, list(CZmap_rail))
  }
}


UEA_map <- UEA_map[c(setdiff(seq_len(length(UEA_map)), seq(1, 4)), seq(1, 4))]
UEA_map.kanto <- UEA_map.kanto[c(setdiff(seq_len(length(UEA_map.kanto)), seq(1, 4)), seq(1, 4))]
CZ_map.kanto <- CZ_map.kanto[c(setdiff(seq_len(length(CZ_map.kanto)), seq(1, 4)), seq(1, 4))]
CZ_map <- CZ_map[c(setdiff(seq_len(length(CZ_map)), seq(1, 4)), seq(1, 4))]


gridExtra::grid.arrange(grobs = UEA_map.kanto, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/Railroad/Harmonized/multiple/1980to2015_UEAmap_kanto.png", bg = "white")
gridExtra::grid.arrange(grobs = CZ_map.kanto, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/Railroad/Harmonized/multiple/1980to2015_CZmap_kanto.png", bg = "white")
gridExtra::grid.arrange(grobs = UEA_map, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/Railroad/Harmonized/multiple/1980to2015_UEAmap.png", bg = "white")
gridExtra::grid.arrange(grobs = CZ_map, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/Railroad/Harmonized/multiple/1980to2015_CZmap.png", bg = "white")

beepr::beep()

