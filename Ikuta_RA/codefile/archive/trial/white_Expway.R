pacman::p_load(
  tidyverse,
  sf,
  patchwork
)

sf::sf_use_s2(FALSE)
White1980.sf <- sf::read_sf("mapdata/mmm19801001/mmm19801001.shp", options = "ENCODING=CP932") %>% 
  dplyr::filter(JISCODE < 47000,
                JISCODE != 13421,
                JISCODE != 1695,
                JISCODE != 1696,
                JISCODE != 1698) %>% 
  dplyr::select() %>% # geometryのみ
  dplyr::summarise() %>% 
  sf::st_transform(4612)

Rail.row <- sf::read_sf("data/N06-23_GML/N06-23_HighwaySection.shp") %>% 
  dplyr::rename(ID = N06_004) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(constructed = as.integer(N06_001),
                start = as.integer(N06_002),
                end = as.integer(N06_003),
                disused = max(end)) %>%
  dplyr::ungroup() %>% 
  sf::st_transform(4612) %>% 
  arrange(ID)

year <- c(seq(1970, 2023, 10), 2023)
map_list <- list()
for (y in year){
  Rail_exist <- Rail.row %>% 
    dplyr::filter(start <= y & end >= y)
  Rail_disused <- Rail.row %>% 
    dplyr::filter(disused < y)
  White1980.sf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(linewidth = 0) +
    ggplot2::geom_sf(data = Rail_disused, color = "red", linewidth = .05) +
    ggplot2::geom_sf(data = Rail_exist, color = "black", linewidth = .05) +
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
  patchwork::plot_annotation(caption = "赤線は廃止･無料開放された道路。",
                             theme = theme(plot.caption = element_text(size = 4)))
ggplot2::ggsave(plot =saveMap, filename = "output/map_image/Railroad/white/1950to2015_Expwaymap.png", dpi = 1200)

