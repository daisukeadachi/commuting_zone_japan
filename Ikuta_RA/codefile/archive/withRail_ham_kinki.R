library(tidyverse)
library(sf)
library(gridExtra)
library(spdep)
library(RColorBrewer)
library(patchwork)
"%not.in%" <- Negate("%in%")



# year <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
# 
# lineMatrix = base::rbind(c(138, 45), c(138, 40), c(130, 37))
# OkinawaLine <- st_linestring(lineMatrix) %>%
#   sf::st_sfc() %>%
#   sf::st_set_crs(4612)
# lineMatrix = base::rbind(c(139.5, 41), c(137.5, 40), c(137.5, 38), c(134, 37), c(130, 37))
# HokkaidoLine <- st_linestring(lineMatrix) %>% 
#   sf::st_sfc() %>% 
#   sf::st_set_crs(4612)
# rm(lineMatrix)
colors <- RColorBrewer::brewer.pal(7, "Set1")
kinki_x = c(134.7, 134.7+2.5)
kinki_y = c(34.1, 35.5)

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
UEA_map.Kinki <- list()
CZ_map <- list()
CZ_map.Kinki <- list()

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
  color_assignment[which(UEA_color$UEA == 27100)] <- colors[1]
  roop <- (1:length(neighbors))[-which(UEA_color$UEA == 27100)]
  
  for (j in roop) {
    available_colors <- setdiff(colors, color_assignment[neighbors[[j]]])
    color_assignment[j] <- available_colors[1]
  }
  UEA_color$color <- color_assignment
  UEA.sf <- UEA_color 
  rm(j, neighbors, color_assignment, available_colors, UEA_color)
  
  CZ_color <- CZ.sf %>%
    dplyr::group_by(cluster) %>%
    dplyr::select(cluster) %>%
    dplyr::summarise() %>%
    sf::st_make_valid()
  EdoCenter <- which(CZ.sf$JISCODE == 27100)
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
  
  
  
  
  ### Rail #####################################################################
  Yr <- year[i] %>% as.numeric()
  Rail <- Rail.row %>% 
    dplyr::filter(end >= Yr,
                  start <= Yr)
  
  SHR <- SHR.row %>% 
    dplyr::filter(end >= Yr,
                  start <= Yr)
  
  # 
  
  ### Kinki ####################################################################
  
  UEA.sf %>% #近畿地方･UEA･編年用
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = muni.sf, fill = "darkgrey", linewidth = 0) +
    ggplot2::geom_sf(aes(fill = color), linewidth = .1, color = "gainsboro") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kinki_y,
                      xlim = kinki_x,
                      datum = NA) +
    ggplot2::labs(caption = paste0("(", year[i], ")"))+
    theme(plot.caption = element_text(size = 5))　-> UEAmap.Kinki
  UEA_map.Kinki <- append(UEA_map.Kinki, list(UEAmap.Kinki))
  
  CZ.sf %>% # 近畿地方･CZ･編年用
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kinki_y,
                      xlim = kinki_x,
                      datum = NA) +
    ggplot2::labs(caption = paste0("(", year[i], ")"))+
    theme(plot.caption    = element_text(size = 5))　-> CZmap.Kinki
  CZ_map.Kinki <- append(CZ_map.Kinki, list(CZmap.Kinki))
  
  UEA.sf %>% #近畿地方･UEA･当年用
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = muni.sf, fill = "darkgrey", linewidth = 0) +
    ggplot2::geom_sf(aes(fill = color), linewidth = .1, color = "gainsboro") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kinki_y,
                      xlim = kinki_x,
                      datum = NA) +
    ggplot2::labs(title = "UEA")+
    theme(plot.title    = element_text(size = 8))　-> UEAmap.Kinki
  CZ.sf %>% # 近畿地方･CZ･当年用
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kinki_y,
                      xlim = kinki_x,
                      datum = NA) +
    ggplot2::labs(title = "CZ") +
    theme(plot.title    = element_text(size = 8))　-> CZmap.Kinki
  
  joinedmap <- CZmap.Kinki + UEAmap.Kinki + 
    patchwork::plot_annotation(
      caption = "この地図は近畿地方のCZ･UEAの塗り分け図に鉄道を載せたものである。点線は新幹線、実線は在来線を示している。\n簡略化のため、市町村境界は表示していない。\nUEAの地図においてグレーとなっているところは、どのUEAにも属さない市町村である。",
      theme = theme(plot.caption = element_text(size = 5, hjust = 0))
    )
  ggplot2::ggsave(joinedmap,filename = paste0("output/map_image/Railroad/harmonized/Kinki/", year[i], "_Kinki_UEAandCZmap.png"), width = 5, height = 3, dpi = 900)
  
  if (i == 5) {
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
    
    EdoCenter <- which(CZ.sf$JISCODE == 27100)
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
    
    rm(j, neighbors, neighbor_matrix, color_assignment, available_colors, CZ_color)
    Rail <- Rail.row %>% 
      dplyr::filter(end >= 1985,
                    start <= 1985)
    SHR <- SHR.row %>% 
      dplyr::filter(end >= 1985,
                    start <= 1985)
    
    CZ.sf %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = color), linewidth = .05, color = "gainsboro") +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
      ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_sf(ylim = kinki_y,
                        xlim = kinki_x,
                        datum = NA) +
      ggplot2::labs(caption = "(1985)")+
      theme(plot.caption = element_text(size = 5))　-> CZmap.Kanto
    CZ_map.kanto <- append(CZ_map.Kinki, list(CZmap.Kanto))
    rm(CZmap.Kanto)
    CZ.sf %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = color), linewidth = .05, color = "gainsboro") +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::geom_sf(data = SHR, color = "#333333", linewidth = .2, linetype = "dashed") +
      ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
      ggplot2::theme_bw() +
      ggplot2::coord_sf(ylim = kinki_y,
                        xlim = kinki_x,
                        datum = NA) +
      ggplot2::labs(caption = "この地図は近畿地方のCZ･UEAの塗り分け図に鉄道を載せたものである。点線は新幹線、実線は在来線を示している。\n市町村の境界については基準化しておらず、それぞれの年のものに従っている。\nなお、市町村境界については簡略化のため省略している。") + 
      ggplot2::theme(legend.position = "none", 
                     plot.caption = element_text(size = 5, hjust = 0)) -> CZ1985
    ggplot2::ggsave(CZ1985, filename = "output/map_image/Railroad/harmonized/Kinki/1985_kanto_CZmap.png", width = 5, height = 3, dpi = 600)
    rm(CZ1985)

    
  }
  
  
}


UEA_map.Kinki <- UEA_map.Kinki[c(setdiff(seq_len(length(UEA_map.Kinki)), seq(1, 4)), seq(1, 4))]
CZ_map.Kinki <- CZ_map.Kinki[c(setdiff(seq_len(length(CZ_map.Kinki)), seq(1, 4)), seq(1, 4))]

map1980to2015 <- patchwork::wrap_plots(CZ_map.Kinki, nrow = 3) +
  patchwork::plot_annotation(
    caption = "この地図は近畿地方のCZの塗り分け地図に鉄道を重ねたものである。点線は新幹線、実線は在来線を示している。\n市町村の境界については基準化しておらず、それぞれの年のものに従っている。なお、市町村境界については簡略化のため省略している。\n東京都千代田区が含まれるCZのみ、すべての年で色を固定して表示しているが、その他のCZは年によって色が異なる場合がある。",
    theme = theme(
      plot.caption = element_text(size = 3, hjust = 0),
    )
  )
ggplot2::ggsave(map1980to2015, filename = "output/map_image/Railroad/harmonized/multiple/1980to2015_CZmap_Kinki.png", bg = "white", dpi = 1200)

map1980to2015 <- patchwork::wrap_plots(UEA_map.Kinki, nrow = 3) +
  patchwork::plot_annotation(
    caption = "この地図は近畿地方のUEAの塗り分け地図に鉄道を重ねたものである。点線は新幹線、実線は在来線を示している。\n市町村の境界については基準化しておらず、それぞれの年のものに従っている。なお、市町村境界については簡略化のため省略している。\n東京都市圏のみ、すべての年で色を固定して表示しているが、その他の都市圏は年によって色が異なる場合がある。\n地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。\n1985年については、UEAのコード表が配布されていないため省いている。",
    theme = theme(
      plot.caption = element_text(size = 3, hjust = 0),
    )
  )
ggplot2::ggsave(map1980to2015, filename = "output/map_image/Railroad/harmonized/multiple/1980to2015_UEAmap_Kinki.png", bg = "white", dpi = 1200)





beepr::beep()

