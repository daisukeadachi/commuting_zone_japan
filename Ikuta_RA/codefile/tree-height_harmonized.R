library(tidyverse)
library(sf)
library(patchwork)
library(spdep)
library(RColorBrewer)

rm(list = ls())

# helper for assigning colors to grouped polygons
source("codefile/color_assignment_impl.R")

"%not.in%" <- Negate("%in%")
colors <- c(RColorBrewer::brewer.pal(5, "Set2"))
# c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
# limitation range

# TODO:関東地方で出す。
lim <- list(x = c(129.3, 142.3), y = c(31.2, 42))
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
# # load previously saved maps if any
# prev_CZ_map <- load_color_map("CZ", dir = color_map_dir)

czlist <- list.files(path = "output/clustered/harmonized", full.names = TRUE, recursive = TRUE) |>
  purrr::set_names(~ stringr::str_remove(basename(.x), "\\.csv$"))


IMGDIR <- "output/map_image/tree-height/harmonized/"
#### map making ####
iwalk(czlist, ~ {
  folder <- stringr::str_split_i(.y, "-", -1)
  if(dir.exists(paste0(IMGDIR, folder)) == FALSE){
    dir.create(paste0(IMGDIR, folder), recursive = TRUE)
  }
  ## map data ##################################################################
  CZ.sf <- muni.sf %>%
    dplyr::left_join(readr::read_csv(.x), by = c("JISCODE" = "i")) %>%
    move_Hokaido()
  ## color assignment ##########################################################)
  EdoCenter <- which(CZ.sf$JISCODE == 13101)
  Gohunai <- CZ.sf$cluster[EdoCenter]
  CZ_color <- assign_group_colors(CZ.sf, "cluster", colors = colors, fixed = list(value = Gohunai, color = RColorBrewer::brewer.pal(6, "Set2")[6]))
  CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>%
    dplyr::select(geometry, color)
  # persist and save CZ signature->color map for next iteration
  # prev_CZ_map <- CZ_color %>% dplyr::select(signature, color)
  # save_color_map(prev_CZ_map, "CZ", dir = color_map_dir)
  rm(CZ_color, EdoCenter, Gohunai)
  ## ggplot zone(for double) #################################################
  CZ.sf %>%
    make_basic_plot(
      lim_x = lim$x,
      lim_y = lim$y,
      HokkaidoLine = TRUE
    ) -> enl
  ggplot2::ggsave(enl,
    filename = paste0(IMGDIR, folder, "/", .y, ".png"),
    device = "png",
    bg = "white"
  )
})

