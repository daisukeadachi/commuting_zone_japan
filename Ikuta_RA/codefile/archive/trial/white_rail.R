pacman::p_load(
  tidyverse,
  sf,
  patchwork,
  ggborderline
)

White1980.sf <- sf::read_sf("mapdata/mmm19801001/mmm19801001.shp", options = "ENCODING=CP932") %>% 
  dplyr::filter(JISCODE < 47000,
                JISCODE != 13421,
                JISCODE != 1695,
                JISCODE != 1696,
                JISCODE != 1698) %>% 
  dplyr::select() %>% # geometryのみ
  sf::st_transform(4612)

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
  dplyr::group_by(ID) %>% 
  dplyr::mutate(constructed = as.integer(N05_004),
                start = as.integer(N05_005b),
                end = as.integer(N05_005e),
                disused = max(end)) %>%
  dplyr::ungroup() %>% 
  dplyr::select(ID, constructed, start, end, disused) %>% 
  sf::st_transform(4612)

year <- c(seq(1950, 2020, 10), 2023)
map_list <- list()
for (y in year){
  Rail_exist <- Rail.row %>% 
    dplyr::filter(start <= y & end >= y)
  Rail_disused <- Rail.row %>% 
    dplyr::filter(disused < y)
  HSR <- HSR.row %>% 
    dplyr::filter(start <= y & end >= y)
  White1980.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(linewidth = .01, color = "black", fill = "white") +
    ggplot2::geom_sf(data = Rail_disused, color = "red", linewidth = .05) +
    ggplot2::geom_sf(data = HSR, color = "#333333", linewidth = .1, alpha = .5, linetype = "dashed") +
    ggplot2::geom_sf(data = Rail_exist, color = "#778899", linewidth = .05) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = y) +
    theme(plot.title    = element_text(size = 4)) -> temp
  map_list <- append(map_list, list(temp))
  rm(temp)
}
saveMap <- patchwork::wrap_plots(map_list, ncol = 3) +
  patchwork::plot_annotation(caption = "黒い実線が在来線、点線が新幹線、赤線が廃線を示す。",
                             theme = theme(plot.caption = element_text(size = 4)))
ggplot2::ggsave(plot =saveMap, filename = "output/map_image/Railroad/white/1950to2015_Railmap.png", dpi = 1200)

map_list <- list()
for (y in year){
  Rail_exist <- Rail.row %>% 
    dplyr::filter(start <= y & end >= y)
  Rail_disused <- Rail.row %>% 
    dplyr::filter(disused < y)
  HSR <- HSR.row %>% 
    dplyr::filter(start <= y & end >= y)
  coords <- st_coordinates(Rail_exist) |> 
    data.frame()
  Rail_exist <- cbind(Rail_exist, coords)
  White1980.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(linewidth = 0) +
    ggplot2::geom_sf(data = Rail_disused, color = "red", linewidth = .05) +
    ggplot2::geom_sf(data = HSR, color = "#333333", linewidth = .1, alpha = .5, linetype = "dashed") +
    # ggborderline::geom_borderline(data = coords, aes(x = X, y = Y),
    #                               lineend = "square", linejoin = "round",
    #                               bordercolour = "#B8FB3C", color = "black",
    #                               borderwidth = .01, linewidth = .05) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = y) +
    theme(plot.title    = element_text(size = 4)) -> temp
  map_list <- append(map_list, list(temp))
  rm(temp)
}
saveMap <- patchwork::wrap_plots(map_list, ncol = 3) +
  patchwork::plot_annotation(caption = "黒い実線が在来線、点線が新幹線、赤線が廃線を示す。",
                             theme = theme(plot.caption = element_text(size = 4)))
ggplot2::ggsave(plot =saveMap, filename = "output/map_image/Railroad/white/1950to2015_Railmap_nonBoundary.png", dpi = 1200)
ggsave(filename = "cover/tempR.png")

