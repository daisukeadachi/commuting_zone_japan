library(tidyverse)
library(sf)
library(patchwork)
library(RColorBrewer)
library(spdep)
rm(list =ls())
# This code generate original CZ map in kinki (the year of municipality data equal to the year of CZ data.) 
# This code only make all year maps in one picture. 

"%not.in%" <- Negate("%in%")
year <- seq(1980, 2020, 5)
colors <- RColorBrewer::brewer.pal(8, "Set2")

# limitation range
kinki_x = c(134.7, 134.7+2.5)
kinki_y = c(34.1, 35.5)

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
    dplyr::filter(JISCODE %in% (18000:31999)) %>% 
    dplyr::select(-NO, -DATE) %>% 
    sf::st_transform(4612)
  CZ.sf <- muni.sf %>% 
    dplyr::left_join(readr::read_csv(czPath, show_col_types = FALSE), by = c("JISCODE" = "i"))
  
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
  
  EdoCenter <- which(CZ.sf$JISCODE == 27100)
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
    dplyr::select(JISCODE, geometry, color)
  rm(j, neighbors, color_assignment, available_colors, CZ_color)
  
  #### map generate (Whole, kinki, kinki) ####
  
  # kinki 
  CZ.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = .05) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kinki_y,
                      xlim = kinki_x,
                      datum = NA) +
    ggplot2::labs(caption = y)+
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 5))　-> CZmap
  maps <- append(maps, list(CZmap))
  rm(CZmap, muni.sf, CZ.sf)
}
somemaps <- patchwork::wrap_plots(maps, nrow = 3) +
  patchwork::plot_annotation(
    caption = "この地図は主に近畿地方についてCZで塗り分けた地図である。市町村の境界については基準化しておらず、それぞれの年のものに従っている。\n大阪市を含むCZのみすべての年で色を固定して表示しているが、その他のCZは年によって色が異なる場合がある。",
    theme = theme(plot.caption = element_text(size = 5, hjust = 0))
  )
ggplot2::ggsave(somemaps, filename = "output/map_image/CZ/multiple/1980to2020_kinki_original_CZmap.png", bg = "white")


####
## harmonized ####
####
maps = list()
muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>% 
  # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
  dplyr::filter(JISCODE %in% (18000:31999)) %>% 
  dplyr::select(-NO, -DATE) %>% 
  sf::st_transform(4612)
for (y in year){
  #### data input ####
  if (y == 2020){
    czPath  = "data/addCZdata/2020_harmonized_small-0.001_tree_height-0.98.csv"
  } else {
    czPath  = paste0("output/", y, "_harmonized.csv")
  }
  
  CZ.sf <- muni.sf %>% 
    dplyr::left_join(readr::read_csv(czPath, show_col_types = FALSE), by = c("JISCODE" = "i"))
  
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
  
  EdoCenter <- which(CZ.sf$JISCODE == 27100)
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
    dplyr::select(JISCODE, geometry, color)
  rm(j, neighbors, color_assignment, available_colors, CZ_color)
  
  #### map generate (Whole, kinki, kinki) ####
  
  # kinki 
  CZ.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = .05) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kinki_y,
                      xlim = kinki_x,
                      datum = NA) +
    ggplot2::labs(caption = y)+
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 5))　-> CZmap
  maps <- append(maps, list(CZmap))
  rm(CZmap, CZ.sf)
}
somemaps <- patchwork::wrap_plots(maps, nrow = 3) +
  patchwork::plot_annotation(
    caption = "この地図は主に近畿地方についてCZで塗り分けた地図である。市町村の境界については2015年で基準化されている。\n大阪市を含むCZのみすべての年で色を固定して表示しているが、その他のCZは年によって色が異なる場合がある。",
    theme = theme(plot.caption = element_text(size = 5, hjust = 0))
  )
ggplot2::ggsave(somemaps, filename = "output/map_image/CZ/multiple/1980to2020_kinki_harmonized_CZmap.png", bg = "white", dpi =900)

