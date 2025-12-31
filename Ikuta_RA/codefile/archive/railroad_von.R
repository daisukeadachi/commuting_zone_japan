pacman::p_load(
  tidyverse,
  sf,
  spdep,
  patchwork
)

# 1950･60･70年時点の鉄道を1980のCZに重ねる。

colors <- RColorBrewer::brewer.pal(7, "Set1")

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

CZmap_1980 <- sf::read_sf("mapdata/mmm19801001/mmm19801001.shp", options = "ENCODING=CP932") %>% 
  dplyr::select(JISCODE) %>% 
  dplyr::left_join(
    readr::read_csv("output/1980_original.csv"), by = c("JISCODE" = "i")
  )

sf::sf_use_s2(FALSE)

CZ_color <- CZmap_1980 %>%
  dplyr::group_by(cluster) %>%
  dplyr::select(cluster) %>%
  dplyr::summarise() %>%
  sf::st_make_valid()
EdoCenter <- which(CZmap_1980$JISCODE == 13101)
Gohunai <- CZmap_1980$cluster[EdoCenter]

neighbors <- spdep::poly2nb(CZ_color)
color_assignment <- rep(NA, length(neighbors))
color_assignment[Gohunai] <- colors[1]
roop <- (1:length(neighbors))[-Gohunai]

for (j in roop) {
  available_colors <- setdiff(colors, color_assignment[neighbors[[j]]])
  color_assignment[j] <- available_colors[1]
}
CZ_color$color <- color_assignment
CZmap_1980 <- CZ_color
rm(j, neighbors, color_assignment, available_colors, CZ_color, EdoCenter, Gohunai)

for (y in c(1950, 1960, 1970)){
  rail <- Rail.row %>% 
    dplyr::filter(start <= y & end >= y)
  hsr <- HSR.row %>% 
    dplyr::filter(start <= y & end >= y)
  CZmap_1980 %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = .05, color = "gainsboro") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = hsr, color = "#333333", linewidth = .2, linetype = "dashed") +
    ggplot2::geom_sf(data = rail, color = "black", linewidth = .1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = paste(y, "CZと鉄道"),
                  caption = "実線が在来線、点線が新幹線を示す。") +
    theme(plot.title    = element_text(size = 8))-> CZmap_rail
  ggplot2::ggsave(plot = CZmap_rail, filename = paste0("output/map_image/Railroad/von/", y, "_CZandRailmap.png"), dpi = 600)
}

for (y in c(1950, 1960, 1970)){
  rail <- Rail.row %>% 
    dplyr::filter(start <= y & end >= y)
  hsr <- HSR.row %>% 
    dplyr::filter(start <= y & end >= y)
  CZmap_1980 %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = .05, color = "gainsboro") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = hsr, color = "#333333", linewidth = .2) +
    ggplot2::geom_sf(data = rail, color = "black", linewidth = .1, linetype = "dashed") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = paste(y, "CZと鉄道"),
                  caption = "点線が在来線、実線が新幹線を示す。") +
    theme(plot.title    = element_text(size = 8))-> CZmap_rail
  ggplot2::ggsave(plot = CZmap_rail, filename = paste0("output/map_image/Railroad/von_alt/", y, "_CZandRailmap.png"), dpi = 600)
}

Rail.row <- sf::read_sf("data/N06-23_GML/N06-23_HighwaySection.shp") %>% 
  dplyr::rename(ID = N06_004) %>% 
  dplyr::mutate(constructed = as.integer(N06_001),
                start = as.integer(N06_002),
                end = as.integer(N06_003)) %>% 
  dplyr::select(ID, constructed, start, end) %>% 
  sf::st_transform(4612)

for (y in c(1950, 1960, 1970)){
  rail <- Rail.row %>% 
    dplyr::filter(start <= y & end >= y)
  hsr <- HSR.row %>% 
    dplyr::filter(start <= y & end >= y)
  CZmap_1980 %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = .05, color = "gainsboro") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_sf(data = rail, color = "black", linewidth = .1) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(31.2, 45.5),
                      xlim = c(129.3, 145.8),
                      datum = NA) +
    ggplot2::labs(title = paste(y, "CZと高速道路")) +
    theme(plot.title    = element_text(size = 8))-> CZmap_rail
  ggplot2::ggsave(plot = CZmap_rail, filename = paste0("output/map_image/Railroad/von/", y, "_CZandExpwaymap.png"), dpi = 600)
}
