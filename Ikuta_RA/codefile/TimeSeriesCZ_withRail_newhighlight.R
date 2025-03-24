library(tidyverse)
library(sf)
library(RColorBrewer)
library(spdep)
rm(list =ls())
# This code generate original CZ map (the year of municipality data equal to the year of CZ data.) 
# This code only make the one year maps. This code make not only simple map but als
# the number of map variation is 4
# whole   : no limitation on map's display range
# enlarge : displays main four islands (Hokkaido･Honshu･Shikoku･Kyushu) with Hokkaido moved to upper side
# kanto   : displays the kanto district
# kinki   : displays the kinki district

"%not.in%" <- Negate("%in%")
year <- seq(1980, 2020, 5)
colors <- RColorBrewer::brewer.pal(8, "Set2")

# limitation range
rail_y = c(31.2, 45.5)
rail_x = c(129.3, 145.8)

# Railroad data
Rail.row <- sf::read_sf("data/N05-23_GML/N05-23_RailroadSection2.shp")
HSR.row <- Rail.row %>% 
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

maps = list()
for (y in year){
  #### data input ####
  if (y == 2020){
    mapPath = "mapdata/mmm20190501/mmm20190501.shp"
    czPath  = "data/addCZdata/2020_original_small-0.001_tree_height-0.98.csv"
  } else {
    mapPath = paste0("mapdata/mmm", y, "1001/mmm", y, "1001.shp")
    czPath  = paste0("output/", y, "_original.csv")
  }
  
  muni.sf <- sf::read_sf(mapPath, options = "ENCODING=CP932") %>% 
    # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
    dplyr::filter(JISCODE %not.in% c(1695, 1696, 1698, 13421)) %>% 
    dplyr::select(-NO, -DATE) %>% 
    sf::st_transform(4612)
  CZlist <- readr::read_csv(czPath, show_col_types = FALSE)
  CZ.sf <- muni.sf %>% 
    dplyr::left_join(CZlist, by = c("JISCODE" = "i"))
  
  #### color assignment ####
  # To use different fill color for each CZ, we assign fill color to each CZ.
  
  # combining all municipalities in the same CZ.
  # to process sf object with dplyr::summarise(), we switch s2 geometry engine off
  sf::sf_use_s2(FALSE) 
  CZ_color <- CZ.sf %>%
    dplyr::group_by(cluster) %>%
    dplyr::select(cluster) %>%
    dplyr::summarise() %>%
    sf::st_make_valid()
  # 
  neighbors <- spdep::poly2nb(CZ_color)
  color_assignment <- rep(NA, length(neighbors))
  for (j in 1:length(neighbors)) {
    available_colors <- lubridate::setdiff(colors, color_assignment[neighbors[[j]]])
    color_assignment[j] <- available_colors[1]
  }
  CZ_color$color <- color_assignment
  CZ.sf <- CZ_color
  rm(j, neighbors, color_assignment, available_colors, CZ_color)
  
  
  #### With railroad maps ####
  
  # In this section, we make the Whole, kanto, and kinki maps with railroad.
  # To save memory, we make railroad maps previously.
  
  Rail <- Rail.row %>% 
    dplyr::filter(end >= y,
                  start <= y)
  newRail <- Rail.row %>% 
    dplyr::filter(end >= y - 5,
                  start <= y - 5)
  HSR <- HSR.row %>% 
    dplyr::filter(end >= y,
                  start <= y)
  
  # Whole
  CZ.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = Rail, color = "white", linewidth = .2) +
    ggplot2::geom_sf(data = Rail, color = "aquamarine", linewidth = .1) +
    ggplot2::geom_sf(data = newRail, color = "black", linewidth = .1) +
    ggplot2::geom_sf(data = HSR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(xlim = rail_x,
                      ylim = rail_y,
                      datum = NA) +
    ggplot2::labs(title = paste(y, "CZと鉄道"))+
    ggplot2::theme(plot.title    = ggplot2::element_text(size = 5))　-> CZmap
  maps <- append(maps, list(CZmap))
  rm(CZmap, Rail, HSR)
}
somemaps <- patchwork::wrap_plots(maps, nrow = 3)  +
  patchwork::plot_annotation(
    #caption = "この地図は主に関東地方についてCZで塗り分けた地図である。市町村の境界については2015年で基準化されている。\n地図の視認性向上のため、北海道の市町村については左上の枠内に表示している。\n東京都千代田区を含むCZのみすべての年で色を固定して表示しているが、その他のCZは年によって色が異なる場合がある。",
    caption = "実線:在来線･点線:新幹線･緑は新路線",
    theme = theme(plot.caption = element_text(size = 5, hjust = 0))
  )   
ggplot2::ggsave(somemaps, filename = "output/map_image/CZ/multiple/1980to2020_withRail_newhighlight_original_CZmap.png", bg = "white", dpi = 900)
rm(somemaps)
print("Original end")

maps = list()
muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>% 
  dplyr::filter(JISCODE %not.in% c(1695, 1696, 1698, 13421)) %>% 
  dplyr::select(-NO, -DATE) %>% 
  sf::st_transform(4612)
# The roop is to make harmonized map 
for (y in year){
  #### data input ####
  if (y == 2020){
    czPath  = "data/addCZdata/2020_harmonized_small-0.001_tree_height-0.98.csv"
  } else {
    czPath  = paste0("output/", y, "_harmonized.csv")
  }
  
  
  #### color assignment ####
  # To use different fill color for each CZ, we assign fill color to each CZ.
  
  # combining all municipalities in the same CZ.
  # to process sf object with dplyr::summarise(), we switch s2 geometry engine off
  sf::sf_use_s2(FALSE) 
  CZ_color <- CZ.sf %>%
    dplyr::group_by(cluster) %>%
    dplyr::select(cluster) %>%
    dplyr::summarise() %>%
    sf::st_make_valid()
  # 
  neighbors <- spdep::poly2nb(CZ_color)
  color_assignment <- rep(NA, length(neighbors))
  for (j in 1:length(neighbors)) {
    available_colors <- lubridate::setdiff(colors, color_assignment[neighbors[[j]]])
    color_assignment[j] <- available_colors[1]
  }
  CZ_color$color <- color_assignment
  CZ.sf <- CZ_color
  rm(j, neighbors, color_assignment, available_colors, CZ_color)
  
  
  #### With railroad maps ####
  
  # In this section, we make the Whole, kanto, and kinki maps with railroad.
  # To save memory, we make railroad maps previously.
  
  Rail <- Rail.row %>% 
    dplyr::filter(end >= y,
                  start <= y)
  newRail <- Rail.row %>% 
    dplyr::filter(end >= y - 5,
                  start <= y - 5)
  HSR <- HSR.row %>% 
    dplyr::filter(end >= y,
                  start <= y)
  
  # Whole
  CZ.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = Rail, color = "white", linewidth = .2) +
    ggplot2::geom_sf(data = Rail, color = "aquamarine", linewidth = .1) +
    ggplot2::geom_sf(data = newRail, color = "black", linewidth = .1) +
    ggplot2::geom_sf(data = HSR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(xlim = rail_x,
                      ylim = rail_y,
                      datum = NA) +
    ggplot2::labs(title = paste(y, "CZと鉄道"))+
    ggplot2::theme(plot.title    = ggplot2::element_text(size = 5))　-> CZmap
  maps <- append(maps, list(CZmap))
  rm(CZmap, Rail, HSR)
}


somemaps <- patchwork::wrap_plots(maps, nrow = 3) +
  patchwork::plot_annotation(
    #caption = "この地図は主に関東地方についてCZで塗り分けた地図である。市町村の境界については2015年で基準化されている。\n地図の視認性向上のため、北海道の市町村については左上の枠内に表示している。\n東京都千代田区を含むCZのみすべての年で色を固定して表示しているが、その他のCZは年によって色が異なる場合がある。",
    caption = "実線:在来線･点線:新幹線･緑は新路線",
    theme = theme(plot.caption = element_text(size = 5, hjust = 0))
  )  
ggplot2::ggsave(somemaps, filename = "output/map_image/CZ/multiple/1980to2020_withRail_newhighlight_harmonized_CZmap.png", bg = "white", dpi = 900)


