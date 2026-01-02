library(tidyverse)
library(sf)
library(patchwork)
library(RColorBrewer)
library(spdep)
rm(list =ls())
# helper for assigning colors to grouped polygons
source("codefile/color_assignment.R")
# This code generate original CZ map in Kanto (the year of municipality data equal to the year of CZ data.) 
# This code only make one file including all year(1980~2020) maps. 

"%not.in%" <- Negate("%in%")
year <- seq(1980, 2020, 5)
colors <- RColorBrewer::brewer.pal(5, "Set2")
# limitation range
kanto_y = c(34.7, 37.1)
kanto_x = c(138, 140.9)

maps = list()

# store signature->color mapping across years
prev_CZ_map <- NULL

# base municipality data
muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>% 
  # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
  dplyr::filter(JISCODE %not.in% c(1695, 1696, 1698, 13421),
                JISCODE %in% (7000:23999)) %>% 
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
  EdoCenter <- which(CZ.sf$JISCODE == 13101)
  Gohunai <- CZ.sf$cluster[EdoCenter]
  CZ_color <- assign_group_colors(CZ.sf, "cluster", colors = colors, fixed = list(value = Gohunai, color = RColorBrewer::brewer.pal(6, "Set2")[6]), prev_colors = prev_CZ_map)
  CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>% 
    dplyr::select(geometry, color)
  # persist for next year
  prev_CZ_map <- CZ_color %>% dplyr::select(signature, color)
  rm(CZ_color, EdoCenter, Gohunai)

  # Kanto 
  CZ.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = .05) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = kanto_y,
                      xlim = kanto_x,
                      datum = NA) +
    ggplot2::labs(caption = y)+
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))　-> CZmap
  maps <- append(maps, list(CZmap))
  if (y == 1980){
    CZ.sf %>% 
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = color), linewidth = .1) +
    ggplot2::scale_fill_identity() +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_sf(ylim = kanto_y,
                        xlim = kanto_x,
                        datum = NA) +
      ggplot2::labs(title = paste0("Commuting zone in Kanto district(", y, ")"))+
      ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))　-> CZmap
    ggplot2::ggsave(filename = "output/map_image/CZ/master/1980_kanto_harmonized_CZmap_eng.png", plot = CZmap)
  }
  rm(CZmap, CZ.sf)
}
somemaps <- patchwork::wrap_plots(maps, nrow = 3) #+
  # patchwork::plot_annotation(
  #   title = "Commuting Zone in Kanto district(1980~2020)",
  #   theme = ggplot2::theme(plot.title = ggplot2::element_text(size=11))
  # )
ggplot2::ggsave(plot = somemaps, filename = "output/map_image/CZ/master/1980to2020_kanto_harmonized_CZmap_eng.png", bg = "white", dpi = 900)
