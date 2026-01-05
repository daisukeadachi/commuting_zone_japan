library(tidyverse)
library(sf)
library(patchwork)
library(spdep)
library(RColorBrewer)

rm(list = ls())

# helper for assigning colors to grouped polygons
source("codefile/color_assignment_impl.R")

"%not.in%" <- Negate("%in%")
colors <- RColorBrewer::brewer.pal(5, "Set2")
# c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
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

# directory to persist signature->color maps across runs
color_map_dir <- "output/color_map"
# load previously saved maps if any
prev_UEA_map <- load_color_map("UEA", dir = color_map_dir)
prev_CZ_map <- load_color_map("CZ", dir = color_map_dir)
for (y in c(1980, 2015)) {
  # ファイルパスをリストで定義
  if (y == 1980) {
    files <- list(
      McEA = "data/UEA/suburb/McEA/McEA80_Rev07.csv",
      McEA.C = "data/UEA/center/McEA/McEA80C_Rev07.csv",
      MEA = "data/UEA/suburb/MEA/MEA80_Rev07.csv",
      MEA.C = "data/UEA/center/MEA/MEA80C_Rev07.csv"
    )
  } else if (y == 2015) {
    files <- list(
      McEA = "data/UEA/suburb/McEA/McEA2005.csv",
      McEA.C = "data/UEA/center/McEA/McEA2005C.csv",
      MEA = "data/UEA/suburb/MEA/MEA2005.csv",
      MEA.C = "data/UEA/center/MEA/MEA2005C.csv"
    )
  }
  
  # リストのファイルを読み込み
  data_list <- purrr::map(files, ~readr::read_csv(.x, 
                                                    locale = readr::locale(encoding = "cp932"), 
                                                    show_col_types = FALSE) %>% 
                            rename_with(~stringr::str_replace(.x, pattern = " ", replacement = "_")) %>% 
                            rename(UEA = 1))
  
  # リストから個別の変数に割り当て
  list2env(data_list, envir = environment())
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
  UEA_color <- assign_group_colors(UEA.sf, "UEA", colors = colors, fixed = list(value = 13100, color = RColorBrewer::brewer.pal(6, "Set2")[6]), prev_colors = prev_UEA_map)
  UEA.sf <- dplyr::left_join(UEA.sf, UEA_color, by = "UEA") %>% 
    dplyr::mutate(color = dplyr::if_else(
      is.na(color), "#a9a9a9", color
    ))
  # persist and save UEA signature->color map
  prev_UEA_map <- UEA_color %>% dplyr::select(signature, color)
  save_color_map(prev_UEA_map, "UEA", dir = color_map_dir)
  rm(UEA_color)
  
  ## CZ ##
  EdoCenter <- which(CZ.sf$JISCODE == 13101)
  Gohunai <- CZ.sf$cluster[EdoCenter]
  CZ_color <- assign_group_colors(CZ.sf, "cluster", colors = colors, fixed = list(value = Gohunai, color = RColorBrewer::brewer.pal(6, "Set2")[6]), prev_colors = prev_CZ_map)
  CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>% 
    dplyr::select(geometry, color)
  # persist and save CZ signature->color map for next iteration
  prev_CZ_map <- CZ_color %>% dplyr::select(signature, color)
  save_color_map(prev_CZ_map, "CZ", dir = color_map_dir)
  rm(CZ_color, EdoCenter, Gohunai)
  ## ggplot zone(for double) #################################################
  UEA.sf %>% 
    make_basic_plot(
      lim_x = lim_x,
      lim_y = lim_y,
      caption = "UEA"
    ) -> UEAmap

  CZ.sf %>% 
    make_basic_plot(
      lim_x = lim_x,
      lim_y = lim_y,
      caption = "CZ"
    ) -> CZmap

  doubleMap <- CZmap + UEAmap + 
    patchwork::plot_annotation(
      title = paste0("CZ and UEA in Kanto district(", y, ")"),
      #caption = "この地図は主に関東地方についてUEAとCZで塗り分けた地図である。市町村の境界については2015年で基準化している。\nそれぞれのUEA及びCZは年によって色が異なる場合があるが、東京都市圏のみすべての年で色を固定して表示している。\n地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。",
      theme = ggplot2::theme(
                #plot.caption = ggplot2::element_text(size = 5, hjust = 0),
                plot.title = ggplot2::element_text(size = 11)
                )
    )
  ggplot2::ggsave(doubleMap, filename = paste0("output/map_image/CZ/master/", y, "_UEAandCZmap_eng.svg"), 
                  device = svg,
                  bg = "white", width = 5, height = 3)
  rm(doubleMap, CZmap, UEAmap, UEA.sf, CZ.sf)

}

