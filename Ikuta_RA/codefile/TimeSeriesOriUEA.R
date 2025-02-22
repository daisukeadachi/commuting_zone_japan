library(tidyverse)
library(sf)
library(patchwork)
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

year <- list.files("data/UEA/suburb/McEA") %>% 
  stringr::str_replace(pattern = "A8", replacement = "A198") %>% 
  stringr::str_replace(pattern = "A9", replacement = "A199") %>% 
  stringr::str_sub(start = 5, end = -5) %>% 
  stringr::str_remove(pattern = "_Rev07")

UEA_Whole <- list()
UEA_kanto <- list()
UEA_Enlarged <- list()


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
      dplyr::select(UEA, suburb4) %>% 
      tidyr::drop_na(suburb4) %>% 
      dplyr::rename(JISCODE = suburb4)
  }
  
  McEA_sub3 <- McEA %>% 
    dplyr::select(UEA, suburb3) %>% 
    tidyr::drop_na(suburb3) %>% 
    dplyr::rename(JISCODE = suburb3) 
  
  McEA_sub2 <- McEA %>% 
    dplyr::select(UEA, suburb2) %>% 
    tidyr::drop_na(suburb2) %>% 
    dplyr::rename(JISCODE = suburb2) 
  
  McEA_sub1 <- McEA %>% 
    dplyr::select(UEA, suburb) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(JISCODE = suburb) 
  
  McEA_center <- McEA.C %>% 
    dplyr::select(UEA, center) %>% 
    dplyr::rename(JISCODE = center) 
  
  if (ncol(MEA) == 20){
    MEA_sub4 <- MEA %>% 
      dplyr::select(UEA, suburb4) %>% 
      tidyr::drop_na(suburb4) %>% 
      dplyr::rename(JISCODE = suburb4)
  }
  
  MEA_sub3 <- MEA %>% 
    dplyr::select(UEA, suburb3) %>% 
    tidyr::drop_na(suburb3) %>% 
    dplyr::rename(JISCODE = suburb3) 
  
  MEA_sub2 <- MEA %>% 
    dplyr::select(UEA, suburb2) %>% 
    tidyr::drop_na(suburb2) %>% 
    dplyr::rename(JISCODE = suburb2)
  
  MEA_sub1 <- MEA %>% 
    dplyr::select(UEA, suburb) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(JISCODE = suburb)
  
  MEA_center <- MEA.C %>% 
    dplyr::select(UEA, center) %>% 
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
  
  mapPath <- paste0("mapdata/mmm", year[i], "1001/mmm", year[i], "1001.shp")
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
  
  ## movement ##################################################################
  
  movement_okinwa <- UEA.sf %>%
    dplyr::filter(JISCODE %in% (47000:47999)) %>% 
    sf::st_set_geometry(sf::st_geometry(UEA.sf %>% dplyr::filter(JISCODE %in% (47000:47999))) + c(5, 15)) %>% 
    sf::st_set_crs(4612)
  
  UEA.sf.whole <- UEA.sf %>% 
    dplyr::filter(JISCODE %not.in% (47000:47999)) %>% 
    dplyr::bind_rows(movement_okinwa) 
  
  movement_Hokkaido <- UEA.sf %>% 
    dplyr::filter(JISCODE %in% (1000:1999)) %>% 
    sf::st_set_geometry(st_geometry(UEA.sf %>% dplyr::filter(JISCODE %in% (1000:1999))) - c(10, 4)) %>% 
    sf::st_set_crs(4612)
  
  UEA.sf.enlarged <- UEA.sf %>% 
    dplyr::filter(JISCODE %not.in% (1000:1999),
                  JISCODE %not.in% (47000:47999)) %>% 
    dplyr::bind_rows(movement_Hokkaido) 
  
  rm(movement_okinwa, movement_Hokkaido, muni.sf)
  
  ## ggplot zone(for multiple) #################################################
  UEA.sf.whole %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = OkinawaLine, linewidth = .1) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(caption = year[i]) +
    ggplot2::theme(plot.caption = element_text(size = 5)) -> add.temp
  UEA_Whole <- append(UEA_Whole, list(add.temp))
  rm(add.temp)
  UEA.sf.enlarged %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = HokkaidoLine, linewidth = .1) +
    ggplot2::coord_sf(ylim = c(31.2, 42),
                      xlim = c(129.3, 142.3),
                      datum = NA) +
    ggplot2::labs(caption = year[i])+
    ggplot2::theme(plot.caption = element_text(size = 5)) -> add.temp
  UEA_Enlarged <- append(UEA_Enlarged, list(add.temp))
  rm(add.temp)
  UEA.sf %>%
    dplyr::filter(JISCODE %not.in% (25000:47999),
                  JISCODE %not.in% (1000:5999)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = .01, color = "gainsboro") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.5, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(caption = year[i])+
    theme(plot.caption    = element_text(size = 5))　-> add.temp
  UEA_kanto <- append(UEA_kanto, list(add.temp))
  rm(add.temp)
  ## ggplot zone(for double) #################################################
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
    ggplot2::labs(caption = "この地図は主に関東地方についてUEAで塗り分けた地図である。市町村の境界については基準化しておらず、それぞれの年のものに従っている。\nそれぞれのUEAは年によって色が異なる場合があるが、東京都市圏のみすべての年で色を固定して表示している。\n地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。")+
    theme(plot.caption    = element_text(size = 3, hjust = 0))　-> UEAmap
  ggplot2::ggsave(UEAmap, filename = paste0("output/map_image/TimeSeries_UEA/Original/Kanto/", year[i], "_UEAmap.png"), 
                  bg = "white", width = 5, height = 3, dpi = 600)
  rm(UEAmap, UEA.sf)
  
  UEA.sf.whole %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = OkinawaLine, linewidth = .1) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(caption = "この地図は国内のほぼ全ての市町村についてUEAで塗り分けた地図である。市町村の境界については基準化しておらず、\nそれぞれの年のものに従っている。地図の簡略化のため、小笠原諸島(東京都小笠原村)及び\n北方領土の一部(北海道色丹郡色丹村･国後郡泊村･留夜別村･択捉郡留別村･紗那郡紗那村･蘂取郡蘂取村)を省いている。\n地図の視認性向上のため、沖縄県の市町村については左上の枠内に表示している(沖縄県の市町村に対してすべて、\n緯度を+5,経度を+15して処理。)。それぞれのUEAは年によって色が異なる場合があるが、東京都市圏のみすべての年で\n色を固定して表示している。地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。") +
    ggplot2::theme(plot.caption = element_text(size = 3, hjust = 0)) -> UEAmap
  ggplot2::ggsave(UEAmap, filename = paste0("output/map_image/TimeSeries_UEA/Original/Whole/", year[i], "_UEAmap.png"), 
                  bg = "white", width = 5, height = 3, dpi = 600)
  rm(UEAmap, UEA.sf.whole)
  
  caption_text <- "この地図は主に北海道･本州･四国･九州についてUEAで塗り分けた地図である。市町村の境界については基準化しておらず、\nそれぞれの年のものに従っている。地図の簡略化のため、小笠原諸島(東京都小笠原村)及び北方領土の一部(北海道色丹郡色丹村･\n国後郡泊村･留夜別村･択捉郡留別村･紗那郡紗那村･蘂取郡蘂取村)を省いている。地図の視認性向上のため、北海道の市町村に\nついては左上の枠内に表示している(北海道の市町村に対してすべて、緯度を-4,経度を-10して処理した。)。それぞれのUEAは\n年によって色が異なる場合があるが、東京都市圏のみすべての年で色を固定して表示している。地図上グレーで塗られた市町村は、\nどのUEAにも含まれない市町村である。"
  UEA.sf.enlarged %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = HokkaidoLine, linewidth = .1) +
    ggplot2::coord_sf(ylim = c(31.2, 42),
                      xlim = c(129.3, 142.3),
                      datum = NA) +
    ggplot2::labs(caption = caption_text)+
    ggplot2::theme(plot.caption = element_text(size = 3, hjust = 0)) -> UEAmap
  ggplot2::ggsave(UEAmap, filename = paste0("output/map_image/TimeSeries_UEA/Original/enlaged_MainLands/", year[i], "_UEAmap.png"), 
                  bg = "white", width = 5, height = 3, dpi = 600)
  rm(UEAmap, UEA.sf.enlarged)
}

# detach("package:spdep")
somemaps <- patchwork::wrap_plots(UEA_Whole, nrow = 3) +
  patchwork::plot_annotation(
    caption = "この地図は国内のほぼ全ての市町村についてUEAで塗り分けた地図である。市町村の境界については基準化しておらず、それぞれの年のものに従っている。\n地図の簡略化のため、小笠原諸島(東京都小笠原村)及び北方領土の一部(北海道色丹郡色丹村･国後郡泊村･留夜別村･択捉郡留別村･紗那郡紗那村･蘂取郡蘂取村)を省いている。\n地図の視認性向上のため、沖縄県の市町村については左上の枠内に表示している(沖縄県の市町村に対してすべて、緯度を+5,経度を+15して処理。)。\n東京都市圏のみ、すべての年で色を固定して表示しているが、その他の都市圏は年によって色が異なる場合がある。\n地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。1985年については、UEAのコード表が配布されていないため省いている。",
    theme = theme(plot.caption = element_text(size = 3, hjust = 0))
  )
ggplot2::ggsave(somemaps, filename = "output/map_image/TimeSeries_UEA/Original/multiple/1980to2015_UEAmap_Whole.png", bg = "white", dpi = 1200)
rm(UEA_Whole, somemaps)

somemaps <- patchwork::wrap_plots(UEA_Enlarged, nrow = 3) +
  patchwork::plot_annotation(
    caption = "この地図は主に北海道･本州･四国･九州についてUEAで塗り分けた地図である。市町村の境界については基準化しておらず、それぞれの年のものに従っている。\n地図の簡略化のため、小笠原諸島(東京都小笠原村)及び北方領土の一部(北海道色丹郡色丹村･国後郡泊村･留夜別村･択捉郡留別村･紗那郡紗那村･蘂取郡蘂取村)を省いている。\n地図の視認性向上のため、北海道の市町村については左上の枠内に表示している(北海道の市町村に対してすべて、緯度を-4,経度を-9して処理した。)。\n東京都市圏のみ、すべての年で色を固定して表示しているが、その他の都市圏は年によって色が異なる場合がある。地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。1985年については、UEAのコード表が配布されていないため省いている。",
    theme = theme(plot.caption = element_text(size = 3, hjust = 0))
  )
ggplot2::ggsave(somemaps, filename = "output/map_image/TimeSeries_UEA/Original/multiple/1980to2015_UEAmap_enlarged.png", bg = "white", dpi = 1200)
rm(UEA_Enlarged, somemaps)

somemaps <- patchwork::wrap_plots(UEA_kanto, nrow = 3) +
  patchwork::plot_annotation(
    caption = "この地図は主に関東地方についてUEAで塗り分けた地図である。市町村の境界については基準化しておらず、それぞれの年のものに従っている。\n地図の視認性向上のため、北海道の市町村については左上の枠内に表示している。\n東京都市圏のみ、すべての年で色を固定して表示しているが、その他の都市圏は年によって色が異なる場合がある。\n地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。1985年については、UEAのコード表が配布されていないため省いている。",
    theme = theme(plot.caption = element_text(size = 3, hjust = 0))
  )
ggplot2::ggsave(somemaps, filename = "output/map_image/TimeSeries_UEA/Original/multiple/1980to2015_UEAmap_kanto.png", bg = "white", dpi = 1200)


library(beepr)
beepr::beep()





