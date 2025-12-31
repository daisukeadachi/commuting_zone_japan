library(tidyverse)
library(sf)
library(patchwork)
library(spdep)
library(RColorBrewer)

rm(list = ls())

"%not.in%" <- Negate("%in%")
colors <- RColorBrewer::brewer.pal(8, "Set1")

# limitation range
lim_y = c(34.7, 37.1)
lim_x = c(138, 140.9)
width = .1

muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>% 
  # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
  dplyr::filter(JISCODE %not.in% c(1695, 1696, 1698, 13421),
                JISCODE %in% (7000:23999)) %>% 
  dplyr::select(-NO, -DATE) %>% 
  sf::st_transform(4612)

for (y in c(1980, 2015)) {
  if (y == 1980) {
    McEA <- readr::read_csv("https://www.csis.u-tokyo.ac.jp/UEA/McEA80_Rev07.csv", locale = readr::locale(encoding = "cp932"), show_col_types = FALSE) %>% 
      rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
      rename(UEA = 1) 
    
    McEA.C <- readr::read_csv("https://www.csis.u-tokyo.ac.jp/UEA/McEA80C_Rev07.csv", locale = readr::locale(encoding = "cp932"), show_col_types = FALSE) %>% 
      rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
      rename(UEA = 1)
    
    MEA <- readr::read_csv("https://www.csis.u-tokyo.ac.jp/UEA/MEA80_Rev07.csv", locale = readr::locale(encoding = "cp932"), show_col_types = FALSE) %>% 
      rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
      rename(UEA = 1)
    
    MEA.C <- readr::read_csv("https://www.csis.u-tokyo.ac.jp/UEA/MEA80C_Rev07.csv", locale = readr::locale(encoding = "cp932"), show_col_types = FALSE) %>% 
      rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
      rename(UEA = 1)
  } 
  if (y == 2015) {
    McEA <- readr::read_csv("https://www.csis.u-tokyo.ac.jp/UEA/McEA2005.csv", locale = readr::locale(encoding = "cp932"), show_col_types = FALSE) %>% 
      rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
      rename(UEA = 1) 
    
    McEA.C <- readr::read_csv("https://www.csis.u-tokyo.ac.jp/UEA/McEA2005C.csv", locale = readr::locale(encoding = "cp932"), show_col_types = FALSE) %>% 
      rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
      rename(UEA = 1)
    
    MEA <- readr::read_csv("https://www.csis.u-tokyo.ac.jp/UEA/MEA2005.csv", locale = readr::locale(encoding = "cp932"), show_col_types = FALSE) %>% 
      rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
      rename(UEA = 1)
    
    MEA.C <- readr::read_csv("https://www.csis.u-tokyo.ac.jp/UEA/MEA2005C.csv", locale = readr::locale(encoding = "cp932"), show_col_types = FALSE) %>% 
      rename_with(\(x) stringr::str_replace(x, pattern = " ", replacement = "_")) %>% 
      rename(UEA = 1)
  }
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
  rm(McEA_center, McEA_sub1, McEA_sub2, McEA_sub3, McEA_sub4,
     MEA_center,  MEA_sub1,  MEA_sub2,  MEA_sub3,  MEA_sub4,
     McEA_center, McEA_sub1, McEA_sub2, McEA_sub3,
     MEA_center,  MEA_sub1,  MEA_sub2,  MEA_sub3,
     McEA.C, McEA, MEA.C, MEA)
  
  #### map making ####
  
  ## map data ##################################################################
  if(y == 1980){
    code <- readr::read_csv("mapdata/codelist_19801001and20151001.csv", locale = locale(encoding = "cp932")) %>%
      dplyr::mutate(JISCODE = as.numeric(JISCODE1),
                    JISCODE_2015 = as.numeric(JISCODE2)) %>% 
      dplyr::select(JISCODE, JISCODE_2015)
    UEA <- UEA %>% 
      dplyr::left_join(code, by = "JISCODE") %>% 
      dplyr::select(-JISCODE) %>% 
      dplyr::rename(JISCODE = JISCODE_2015) %>% 
      dplyr::distinct()
  }
  czPath <- paste0("output/", y, "_harmonized.csv")
  UEA.sf <- muni.sf %>% 
    dplyr::mutate(JISCODE = dplyr::if_else((stringr::str_sub(CNAME, -1, -1) == "区" & PNAME == "東京都"), 
                                           base::trunc(JISCODE * 0.01) * 100 , 
                                           JISCODE
    )
    ) %>% 
    dplyr::left_join(UEA, by = "JISCODE")
  CZ.sf <- muni.sf %>% 
    dplyr::left_join(readr::read_csv(czPath), by = c("JISCODE" = "i"))
  
  ## color assignment ##########################################################
  
  sf_use_s2(FALSE) 
  
  ## UEA ##
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
  UEA.sf <- dplyr::left_join(UEA.sf, UEA_color, by = "UEA")
  
  rm(j, neighbors, color_assignment, available_colors, UEA_color)
  
  ## CZ ##
  CZ_color <- CZ.sf %>%
    dplyr::group_by(cluster) %>%
    dplyr::select(cluster) %>%
    dplyr::summarise() %>%
    sf::st_make_valid()

  EdoCenter <- which(CZ.sf$JISCODE == 13101)
  Gohunai <- CZ.sf$cluster[EdoCenter]
  edo <- which(CZ_color$cluster == Gohunai)
  
  neighbors <- spdep::poly2nb(CZ_color)
  color_assignment <- rep(NA, length(neighbors))
  color_assignment[edo] <- colors[1]
  roop <- (1:length(neighbors))[-edo]
  for (j in roop) {
    available_colors <- lubridate::setdiff(colors, color_assignment[neighbors[[j]]])
    color_assignment[j] <- available_colors[1]
  }
  CZ_color$color <- color_assignment
  CZ_color <- CZ_color %>%
    dplyr::tibble() %>%
    dplyr::select(-geometry)
  CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>% 
    dplyr::select(geometry, color)
  rm(j, neighbors, color_assignment, available_colors, CZ_color)
  ## ggplot zone(for double) #################################################
  UEA.sf %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color), linewidth = width) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = lim_y,
                      xlim = lim_x,
                      datum = NA) +
    ggplot2::labs(caption = "UEA")+
    theme(plot.caption    = element_text(size = 10))　-> UEAmap
  CZ.sf %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color), linewidth = width) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = lim_y,
                      xlim = lim_x,
                      datum = NA) +
    ggplot2::labs(caption = "CZ")+
    theme(plot.caption    = element_text(size = 10))　-> CZmap
  doubleMap <- CZmap + UEAmap + 
    patchwork::plot_annotation(
      title = paste0("CZ and UEA in Kanto district(", y, ")"),
      #caption = "この地図は主に関東地方についてUEAとCZで塗り分けた地図である。市町村の境界については2015年で基準化している。\nそれぞれのUEA及びCZは年によって色が異なる場合があるが、東京都市圏のみすべての年で色を固定して表示している。\n地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。",
      theme = ggplot2::theme(
                #plot.caption = ggplot2::element_text(size = 5, hjust = 0),
                plot.title = ggplot2::element_text(size = 11)
                )
    )
  ggplot2::ggsave(doubleMap, filename = paste0("output/map_image/CZ/master/", y, "_UEAandCZmap_eng.png"), 
                  bg = "white", width = 5, height = 3)
  rm(doubleMap, CZmap, UEAmap, UEA.sf, CZ.sf)

}