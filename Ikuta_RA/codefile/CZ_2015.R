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
lim <- list(
  enlarge = list(x = c(129.3, 142.3), y = c(31.2, 42)),
  kanto = list(x = c(138, 140.9), y = c(34.7, 37.1))
)
width <- .1
outdir <- "output/map_image/CZ2015/"



muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>%
  # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
  dplyr::filter(
    JISCODE %not.in% c(1695, 1696, 1698, 13421)
  ) %>%
  dplyr::select(-NO, -DATE) %>%
  sf::st_transform(4612)

# directory to persist signature->color maps across runs
color_map_dir <- "output/color_map"
# load previously saved maps if any
prev_CZ_map <- load_color_map("CZ", dir = color_map_dir)

czPath <- paste0("output/2015_harmonized.csv")
#### map making ####

## map data ##################################################################
CZ.sf <- muni.sf %>%
  dplyr::left_join(readr::read_csv(czPath), by = c("JISCODE" = "i")) %>%
  move_Hokaido()

## color assignment ##########################################################

sf_use_s2(FALSE)

## CZ ##
EdoCenter <- which(CZ.sf$JISCODE == 13101)
Gohunai <- CZ.sf$cluster[EdoCenter]
CZ_color <- assign_group_colors(CZ.sf, "cluster", colors = colors, fixed = list(value = Gohunai, color = RColorBrewer::brewer.pal(6, "Set2")[6]), prev_colors = prev_CZ_map)
CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>%
  dplyr::select(geometry, color, JISCODE) %>%
  dplyr::mutate(color = dplyr::if_else(
    is.na(color), "#a9a9a9", color
  ))

# persist and save CZ signature->color map for next iteration
prev_CZ_map <- CZ_color %>% dplyr::select(signature, color)
save_color_map(prev_CZ_map, "CZ", dir = color_map_dir)
rm(CZ_color, EdoCenter, Gohunai)
## ggplot zone(for double) #################################################
CZ.sf %>%
  make_basic_plot(
    lim_x = lim$enlarge$x,
    lim_y = lim$enlarge$y,
    HokkaidoLine = TRUE,
    pref_boundary = 0.3
  ) -> enl
ggplot2::ggsave(enl,
  filename = paste0(outdir, "2015_CZmap_enlarge.png"),
  bg = "white",
  create.dir = TRUE
)
CZ.sf %>%
  dplyr::filter(JISCODE %in% (7000:23999)) %>%
  make_basic_plot(
    lim_x = lim$kanto$x,
    lim_y = lim$kanto$y,
    pref_boundary = 0.3
  ) -> kanto
ggplot2::ggsave(kanto,
  filename = paste0(outdir, "2015_CZmap_kanto.png"),
  bg = "white"
)

# doubleMap <- enl + UEAmap +
#   patchwork::plot_annotation(
#     title = paste0("CZ and UEA in Kanto district(", y, ")"),
#     # caption = "この地図は主に関東地方についてUEAとCZで塗り分けた地図である。市町村の境界については2015年で基準化している。\nそれぞれのUEA及びCZは年によって色が異なる場合があるが、東京都市圏のみすべての年で色を固定して表示している。\n地図上グレーで塗られた市町村は、どのUEAにも含まれない市町村である。",
#     theme = ggplot2::theme(
#       # plot.caption = ggplot2::element_text(size = 5, hjust = 0),
#       plot.title = ggplot2::element_text(size = 11)
#     )
#   )
# ggplot2::ggsave(doubleMap,
#   filename = paste0("output/map_image/CZ/master/", y, "_UEAandCZmap_eng.svg"),
#   device = svg,
#   bg = "white", width = 5, height = 3
# )
