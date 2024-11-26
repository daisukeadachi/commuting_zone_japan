library(tidyverse)
library(sf)
library(gridExtra)
library(RColorBrewer)
library(spdep)


year <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)

lineMatrix = base::rbind(c(139.5, 41), c(137.5, 40), c(137.5, 38), c(134, 37), c(130, 37))
HokkaidoLine <- st_linestring(lineMatrix) %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4612)

colors <- RColorBrewer::brewer.pal(8, "Set2")


#harmonized (2015)--------------------------------------------------------------
mapdata <- sf::read_sf("mapdata/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>% 
  dplyr::select(-NO, -DATE) %>% 
  sf::st_transform(4612)


sf_use_s2(FALSE) 

## elminate islands
# muni_neighbors <- spdep::poly2nb(mapdata) %>% 
#   spdep::nb2mat(style = "B", zero.policy = TRUE) %>% 
#   base::rowSums()
# mapdata$neighbors <- muni_neighbors
# 
# island_munis <- c(1518, 1519, 28226, 28205, 28224, 28685, 37321, 37323, 37322,
#                   37364, 34206, 34430, 46213, 43212, 43527, 43523, 43207, 43530,
#                   43531 ,43532 , 43533, 43209, 46403, 46404, 46207)
# 
# mapdata <- mapdata %>% 
#   dplyr::filter(neighbors != 0 ,
#                 !(JISCODE %in% (46501:47999)),
#                 !(JISCODE %in% island_munis)) 

sf_use_s2(TRUE) 

# 北海道を動かす
Hokkaido <- mapdata %>% 
  dplyr::filter(JISCODE %in% (1000:1999)) %>% 
  sf::st_set_geometry(st_geometry(mapdata %>% dplyr::filter(JISCODE %in% (1000:1999))) - c(10, 4)) %>% #将来的？:c(17, ?)で日本列島の南側に移せる(北海道を移すスペースを確保できる)
  sf::st_set_crs(4612)

mapdata <- mapdata %>% 
  dplyr::filter(!(JISCODE %in% (1000:1999))) %>% 
  dplyr::bind_rows(Hokkaido)




for (y in year) {
  cz_data <- base::paste0("output/", y, "_harmonized.csv")
  
  cz_map <- dplyr::left_join(mapdata, readr::read_csv(cz_data), by = c("JISCODE" = "i"))

  sf_use_s2(FALSE) 
  
  cz_color <- cz_map %>%
    dplyr::group_by(cluster) %>%
    dplyr::select(cluster) %>%
    dplyr::summarise() %>%
    sf::st_make_valid()
  
  neighbors <- spdep::poly2nb(cz_color)
  neighbor_matrix <- spdep::nb2mat(neighbors, style = "B", zero.policy = TRUE)
  color_assignment <- rep(NA, length(neighbors))
  
  for (i in 1:length(neighbors)) {
    available_colors <- setdiff(colors, color_assignment[neighbors[[i]]])
    color_assignment[i] <- available_colors[1]
  }
  
  cz_color$color <- color_assignment
  
  cz_color <- cz_color %>%
    dplyr::tibble() %>%
    select(-geometry)
  
  cz_map <- cz_map %>% 
    dplyr::left_join(cz_color, by = "cluster")
  
  base::rm(color_assignment, i, neighbors, neighbor_matrix, cz_color)
  sf_use_s2(TRUE)
  
  
  table_name <- base::paste0("Commuting Zone(", y, ")")
  table_name_kanto <- base::paste0("関東・Commuting Zone(", y, ")")
  cz_map %>%   
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = HokkaidoLine) +
    ggplot2::coord_sf(ylim = c(31.2, 42),
                      xlim = c(129.3, 142.3),
                      datum = NA) +
    ggplot2::labs(title = table_name) +
    ggplot2::theme(plot.title    = element_text(size = 5)) -> map
  cz_map %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.05, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.5, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = table_name_kanto)+
    ggplot2::theme(plot.title    = element_text(size = 5)) -> map_kanto
  
  fileName <- base::paste0("output/map_image/featured/TimeSeriesCZ/individual/", y, "_CZmap_harmonized.png")
  fileNameKanto <- base::paste0("output/map_image/featured/TimeSeriesCZ/individual/", y, "_CZmap_harmonized_Kanto.png")
  ggplot2::ggsave(map, filename = fileName, bg = "white")
  # ggplot2::ggsave(map_kanto, filename = fileNameKanto, bg = "white")
  map_name <- base::paste0("CZ_", y)
  base::assign(map_name, map)
  map_nameKanto <- base::paste0("CZ_", y, "_Kanto")
  base::assign(map_nameKanto, map_kanto)
  
}


gridExtra::grid.arrange(CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/featured/TimeSeriesCZ/grid/1980to2015_CZmap_harmonized.png", bg = "white")

# gridExtra::grid.arrange(CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto,
#                         CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto, nrow = 3) %>%
#   ggplot2::ggsave(filename = "output/map_image/featured/TimeSeriesCZ/grid/1980to2015_CZmap_harmonized_kanto.png", bg = "white")

base::rm(map_nameKanto, map_name, fileNameKanto, fileName, cz_map, map, table_name_kanto, table_name,
         cz_data, y, temp_fill, mapdata, map_kanto, cz_okinawa,
         CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015,
         CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto, 
         CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto)

#original-----------------------------------------------------------------------

for (y in year) {
  mapPath <- base::paste0("mapdata/mmm", y, "1001/mmm", y, "1001.shp")
  mapdata <- sf::read_sf(mapPath, options = "ENCODING=CP932") %>% 
    dplyr::select(-NO, -DATE) %>% 
    sf::st_transform(4612)
  Hokkaido <- mapdata %>% 
    dplyr::filter(JISCODE %in% (1000:1999)) %>% 
    sf::st_set_geometry(st_geometry(mapdata %>% dplyr::filter(JISCODE %in% (1000:1999))) - c(10, 4)) %>% #将来的？:c(17, ?)で日本列島の南側に移せる(北海道を移すスペースを確保できる)
    sf::st_set_crs(4612)
  
  mapdata <- mapdata %>% 
    dplyr::filter(!(JISCODE %in% (1000:1999))) %>% 
    dplyr::bind_rows(Hokkaido)
  
  czPath <- base::paste0("output/", y, "_original.csv")
  cz_map <- dplyr::left_join(mapdata, readr::read_csv(czPath), by = c("JISCODE" = "i"))

  sf_use_s2(FALSE)

  cz_color <- cz_map %>%
    dplyr::group_by(cluster) %>%
    dplyr::select(cluster) %>%
    dplyr::summarise() %>%
    sf::st_make_valid()

  neighbors <- spdep::poly2nb(cz_color)
  neighbor_matrix <- spdep::nb2mat(neighbors, style = "B", zero.policy = TRUE)
  color_assignment <- rep(NA, length(neighbors))

  for (i in 1:length(neighbors)) {
    available_colors <- setdiff(colors, color_assignment[neighbors[[i]]])
    color_assignment[i] <- available_colors[1]
  }

  cz_color$color <- color_assignment

  cz_color <- cz_color %>%
    dplyr::tibble() %>%
    select(-geometry)

  cz_map <- cz_map %>%
    dplyr::left_join(cz_color, by = "cluster")

  base::rm(color_assignment, i, neighbors, neighbor_matrix, cz_color, available_colors)
  sf_use_s2(TRUE)

  tableName <- base::paste0("Commuting Zone(", y, ")")
  tableNameKanto <- base::paste0("関東・Commuting Zone(", y, ")")
  cz_map %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.01, color = "white") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_sf(data = HokkaidoLine) +
    ggplot2::coord_sf(ylim = c(31.2, 42),
                      xlim = c(129.3, 142.3),
                      datum = NA) +
    ggplot2::labs(title = tableName) +
    theme(plot.title    = element_text(size = 5)) -> map
  cz_map %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = color), linewidth = 0.05) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.5, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = tableNameKanto)+
    theme(plot.title    = element_text(size = 5)) -> map_kanto

  fileName <- base::paste0("output/map_image/featured/TimeSeriesCZ/individual/", y, "_CZmap_original.png")
  fileNameKanto <- base::paste0("output/map_image/featured/TimeSeriesCZ/individual/", y, "_CZmap_original_Kanto.png")
  ggplot2::ggsave(map, filename = fileName, bg = "white")
  # ggplot2::ggsave(map_kanto, filename = fileNameKanto, bg = "white")
  map_name <- base::paste0("CZ_", y)
  base::assign(map_name, map)
  map_nameKanto <- base::paste0("CZ_", y, "_Kanto")
  base::assign(map_nameKanto, map_kanto)
}

gridExtra::grid.arrange(CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/featured/TimeSeriesCZ/grid/1980to2015_CZmap_original.png", bg = "white")

# gridExtra::grid.arrange(CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto,
#                         CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto, nrow = 3) %>%
#   ggplot2::ggsave(filename = "output/map_image/featured/TimeSeriesCZ/grid/1980to2015_CZmap_original_kanto.png", bg = "white")

base::rm(map_nameKanto, map_name, fileNameKanto, fileName, cz_map, map, tableName, tableNameKanto,
         czPath, mapPath, y, map_kanto,
         CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015,
         CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto,
         CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto,
         year, lineMatrix, colors, HokkaidoLine)

