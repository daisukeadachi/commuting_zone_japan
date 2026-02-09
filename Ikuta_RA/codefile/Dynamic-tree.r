# 2015_harmonized_small-0.001_tree_height-0.81

library(tidyverse)
library(sf)
library(patchwork)
library(spdep)
library(RColorBrewer)
library(leaflet)
rm(list = ls())

# helper for assigning colors to grouped polygons
source("codefile/color_assignment_impl.R")

"%not.in%" <- Negate("%in%")
colors <- RColorBrewer::brewer.pal(5, "Set2")


"%not.in%" <- Negate("%in%")

gen_map <- function(YEAR = 2015, TreeHeight = "0.81", harmonized = FALSE, overlaymap = NULL) {
  map_path <- stringr::str_glue("mapdata/mmm{YEAR}1001/mmm{YEAR}1001.shp")
  if (harmonized) {
    map_path <- stringr::str_glue("mapdata/mmm20151001/mmm20151001.shp")
  }
  muni.sf <- sf::read_sf(map_path, options = "ENCODING=CP932") %>%
    # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
    dplyr::filter(
      JISCODE %not.in% c(1695, 1696, 1698, 13421)
    ) %>%
    dplyr::select(-NO, -DATE) %>%
    sf::st_transform(4612)
  if (harmonized) {
    czPath <- stringr::str_glue("output/clustered/harmonized/{TreeHeight}/{YEAR}_harmonized_small-0.001_tree_height-{TreeHeight}.csv")
  } else {
    czPath <- stringr::str_glue("output/clustered/original/{TreeHeight}/{YEAR}_original_small-0.001_tree_height-{TreeHeight}.csv")
  }
  #### map making ####
  ## map data ##################################################################
  CZ.sf <- muni.sf %>%
    dplyr::left_join(readr::read_csv(czPath), by = c("JISCODE" = "i"))
  EdoCenter <- which(CZ.sf$JISCODE == 13101)
  Gohunai <- CZ.sf$cluster[EdoCenter]
  CZ_color <- assign_group_colors(CZ.sf, "cluster", colors = colors, fixed = list(value = Gohunai, color = RColorBrewer::brewer.pal(6, "Set2")[6]))
  CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>%
    dplyr::mutate(color = dplyr::if_else(
      is.na(color), "#a9a9a9", color
    ))
  if (!is.null(overlaymap)) {
    overlaymap <- sf::read_sf(overlaymap, options = "ENCODING=CP932") %>%
      sf::st_transform(4612) %>%
      sf::st_cast("MULTILINESTRING")
  }
  leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = CZ.sf,
      popup = ~ paste0("<b>", PNAME, NAME, "</b><br>Cluster: ", cluster),
      color = "#444444",
      weight = 1,
      opacity = 0.7,
      fillOpacity = 0.5,
      fillColor = ~color
    ) %>% 
    {
      if (!is.null(overlaymap)) {
        addPolylines(
          .,
          data = overlaymap,
          color = "#ff0000",
          weight = 2.5,
          opacity = 1
        )
      } else {
        .
      }
    }
}

gen_map(
  YEAR = 2015,
  TreeHeight = "0.98",
  harmonized = TRUE,
  overlaymap = "output/shpfiles/main/2015/CZ_2015.shp"
)|>print()
