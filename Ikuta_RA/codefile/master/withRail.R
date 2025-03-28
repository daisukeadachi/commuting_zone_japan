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
# year <- seq(1980, 2020, 5)
y = 2015
# this linestring is used in enlarge map to separate Hokkaido area with others 
HokkaidoLine <- rbind(c(137.5, 45), c(137.5, 40), c(134, 37), c(120, 37)) %>% 
  sf::st_linestring() %>% 
  sf::st_sfc(crs = 4612) %>% 
  sf::st_sf()
colors <- RColorBrewer::brewer.pal(8, "Set1")

# limitation range
kanto_y = c(34.7, 37.1)
kanto_x = c(138, 140.9)
nagoya_x = c(134.7+2-0.5, 138+0.5)
nagoya_y = c(34.1+.2, 35.5+.5)
kinki_x = c(134.7, 134.7+2)
kinki_y = c(34.1, 35.5)
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


muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>% 
  dplyr::filter(JISCODE %not.in% c(1695, 1696, 1698, 13421)) %>% 
  dplyr::select(-NO, -DATE) %>% 
  sf::st_transform(4612)
# The roop is to make harmonized map 
# for (y in year){
  #### data input ####
  if (y == 2020){
    czPath  = "data/addCZdata/2020_harmonized_small-0.001_tree_height-0.98.csv"
  } else {
    czPath  = paste0("output/", y, "_harmonized.csv")
  }
  
  # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
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
  CZ_color <- CZ_color %>%
    dplyr::tibble() %>%
    dplyr::select(-geometry)
  CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>% 
    dplyr::select(JISCODE, geometry, color)
  rm(j, neighbors, color_assignment, available_colors, CZ_color)
  
  
  #### With railroad maps ####
  
  # In this section, we make the Whole, kanto, and kinki maps with railroad.
  # To save memory, we make railroad maps previously.
  
  Rail <- Rail.row %>% 
    dplyr::filter(end >= y,
                  start <= y)
  HSR <- HSR.row %>% 
    dplyr::filter(end >= y,
                  start <= y)
  
  # Kanto 
  CZ.sf %>% 
    dplyr::filter(JISCODE %in% (7000:23999)) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = HSR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kanto_y,
                      xlim = kanto_x,
                      datum = NA) +
    ggplot2::labs(title = paste0("CZ and railroad in Kanto district(", y, ")"))+
    ggplot2::theme(plot.title    = ggplot2::element_text(size = 10))　-> CZmap
  filepath = paste0("output/map_image/CZ/master/", y, "_kanto_CZwithRailmap_eng.png")
  ggplot2::ggsave(filename = filepath, plot = CZmap)
  rm(CZmap, kantoCZ)
  
  # kinki
  CZ.sf %>%
    dplyr::filter(JISCODE %in% (18000:31999)) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = HSR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kinki_y,
                      xlim = kinki_x,
                      datum = NA) +
    ggplot2::labs(title = paste("CZ and railroad in Kinki district(", y, ")"))+
    ggplot2::theme(plot.title    = ggplot2::element_text(size = 10))　-> CZmap
  filepath = paste0("output/map_image/CZ/master/", y, "_kinki_CZwithRailmap_eng.png")
  ggplot2::ggsave(filename = filepath, plot = CZmap)
  rm(CZmap)
  
  # nagoya
  CZ.sf %>%
    dplyr::filter(JISCODE %in% (16000:25999)) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = HSR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .2) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = nagoya_y,
                      xlim = nagoya_x,
                      datum = NA) +
    ggplot2::labs(title = paste("CZ and railroad in Tokai district(", y, ")"))+
    ggplot2::theme(plot.title    = ggplot2::element_text(size = 10))　-> CZmap
  filepath = paste0("output/map_image/CZ/master/", y, "_nagoya_CZwithRailmap_eng.png")
  ggplot2::ggsave(filename = filepath, plot = CZmap)
  rm(CZmap)
  
  # Whole
  CZ.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = HSR, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail, color = "white", linewidth = .2) +
    ggplot2::geom_sf(data = Rail, color = "black", linewidth = .1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(xlim = rail_x,
                      ylim = rail_y,
                      datum = NA) +
    ggplot2::labs(title = paste("CZ and railroad (", y, ")"))+
    ggplot2::theme(plot.title    = ggplot2::element_text(size = 10))　-> CZmap
  filepath = paste0("output/map_image/CZ/master/", y, "_whole_CZwithRailmap_eng.png")
  ggplot2::ggsave(filename = filepath, plot = CZmap, dpi = 600)
  rm(CZmap, Rail, HSR)
  
# }




