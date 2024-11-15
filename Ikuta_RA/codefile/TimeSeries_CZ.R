library(tidyverse)
library(sf)
library(gridExtra)


#2005---------------------------------------------------------------------------
# 
CZ_2005 <- readr::read_csv("output/2005_original.csv")


CZ_2005.sf <- sf::read_sf("mapdata/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>%
  dplyr::left_join(CZ_2005, by = c("JISCODE" = "i"))
CZ_2005.sf <- CZ_2005.sf %>%
  dplyr::left_join(CZ_2005.sf %>%
                     dplyr::tibble() %>%
                     dplyr::select(cluster, NAME) %>%
                     dplyr::group_by(cluster) %>%
                     dplyr::slice_head(n = 1) %>%
                     dplyr::rename(fill = NAME),
                   by = "cluster") %>% 
  sf::st_set_crs(4612)

CZ_Okinawa <- CZ_2005.sf %>% 
  dplyr::filter(JISCODE %in% (47000:47999)) %>% 
  mutate(geometry = st_geometry(CZ_2005.sf %>% dplyr::filter(JISCODE %in% (47000:47999))) + c(0, 15))  %>% 
  sf::st_set_crs(4612)



CZ_2005.sf %>%
  dplyr::filter(JISCODE != 13421,
                !(JISCODE %in% (47000:47999))) %>% 
  dplyr::bind_rows(CZ_Okinawa) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = fill), linewidth = 0.01, color = "gray") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(title = "CZ_alt") -> CZmap_2005

CZ_2005.sf %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = fill)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(ylim = c(34, 37),
                    xlim = c(138, 141),
                    datum = NA) +
  ggplot2::labs(title = "関東地方・Commuting Zone(2005)")+
  ggplot2::theme(plot.title    = element_text(size = 10)) -> CZmap_2005_Kanto

ggplot2::ggsave(filename = "output/mapB.png", bg = "white")
# 

year <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)


#harmonized (2015)--------------------------------------------------------------

mapdata <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") 

for (i in year) {
  cz_data <- base::paste0("output/", i, "_harmonized.csv")
  
  cz_map <- dplyr::left_join(mapdata, readr::read_csv(cz_data), by = c("JISCODE" = "i"))
  cz_map %>% 
    dplyr::tibble() %>% 
    dplyr::select(cluster, NAME) %>% 
    dplyr::group_by(cluster) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::rename(fill = NAME) -> temp_fill
  cz_map <- dplyr::left_join(cz_map, temp_fill, by = "cluster")
  
  table_name <- base::paste0("Commuting Zone(", i, ")")
  table_name_kanto <- base::paste0("関東・Commuting Zone(", i, ")")
  cz_map %>%   
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = fill), linewidth = 0.01, color = "white") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(#ylim = c(24, 45.5),
                      #xlim = c(122.9, 149),
                      datum = NA) +
    ggplot2::labs(title = table_name) -> map
  cz_map %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = fill)) +
    ggplot2::theme_bw() +
    # ggplot2::scale_fill_brewer(type = "qua") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.5, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = table_name_kanto)+
    theme(plot.title    = element_text(size = 10)) -> map_kanto
  
  fileName <- base::paste0("output/map_image/TimeSeriesCZ/", i, "_CZmap_harmonized.png")
  fileNameKanto <- base::paste0("output/map_image/TimeSeriesCZ/", i, "_CZmap_harmonized_Kanto.png")
  # ggplot2::ggsave(map, filename = fileName, bg = "white")
  # ggplot2::ggsave(map_kanto, filename = fileNameKanto, bg = "white")
  map_name <- base::paste0("CZ_", i)
  base::assign(map_name, map)
  map_nameKanto <- base::paste0("CZ_", i, "_Kanto")
  base::assign(map_nameKanto, map_kanto)
  
}


gridExtra::grid.arrange(CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015, nrow = 3) %>% 
  ggplot2::ggsave(filename = "output/map_image/TimeSeriesCZ/1980to2015_CZmap_harmonized.png", bg = "white")

gridExtra::grid.arrange(CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto, 
                        CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto, nrow = 3) %>% 
  ggplot2::ggsave(filename = "output/map_image/TimeSeriesCZ/1980to2015_CZmap_harmonized_kanto.png", bg = "white")

base::rm(map_nameKanto, map_name, fileNameKanto, fileName, cz_map, map, table_name_kanto, table_name,
         cz_data, i, temp_fill, mapdata, map_kanto,
         CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015,
         CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto, 
         CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto,)

#original-----------------------------------------------------------------------

for (i in year) {
  mapPath <- base::paste0("mapdata/mmm", i, "1001/mmm", i, "1001.shp")
  czPath <- base::paste0("output/", i, "_original.csv")
  
  cz_map <- sf::read_sf(mapPath, options = "ENCODING=CP932") %>% 
    dplyr::left_join(readr::read_csv(czPath), by = c("JISCODE" = "i"))
  temp_fill <- cz_map %>% 
    dplyr::tibble() %>% 
    dplyr::select(cluster, NAME) %>% 
    dplyr::group_by(cluster) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::rename(fill = NAME)
  cz_map <- dplyr::left_join(cz_map, temp_fill, by = "cluster")
  
  tableName <- base::paste0("Commuting Zone(", i, ")")
  tableNameKanto <- base::paste0("関東・Commuting Zone(", i, ")")
  cz_map %>%   
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = fill), linewidth = 0.01, color = "white") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(#ylim = c(24, 45.5),
                      #xlim = c(122.9, 149),
                      datum = NA) +
    ggplot2::labs(title = tableName) +
    theme(plot.title    = element_text(size = 3)) -> map
  cz_map %>% 
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = fill), linewidth = 0.05) +
    ggplot2::theme_bw() +
    # ggplot2::scale_fill_brewer(type = "qua") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_sf(ylim = c(34.5, 37.1),
                      xlim = c(138, 141),
                      datum = NA) +
    ggplot2::labs(title = tableNameKanto)+
    theme(plot.title    = element_text(size = 3)) -> map_kanto
  
  fileName <- base::paste0("output/map_image/TimeSeriesCZ/", i, "_CZmap_original.png")
  fileNameKanto <- base::paste0("output/map_image/TimeSeriesCZ/", i, "_CZmap_original_Kanto.png")
  # ggplot2::ggsave(map, filename = fileName, bg = "white")
  # ggplot2::ggsave(map_kanto, filename = fileNameKanto, bg = "white")
  map_name <- base::paste0("CZ_", i)
  base::assign(map_name, map)
  map_nameKanto <- base::paste0("CZ_", i, "_Kanto")
  base::assign(map_nameKanto, map_kanto)
}

gridExtra::grid.arrange(CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015, nrow = 3) %>%
  ggplot2::ggsave(filename = "output/map_image/TimeSeriesCZ/1980to2015_CZmap_original.png", bg = "white")

gridExtra::grid.arrange(CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto, 
                        CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto, nrow = 3) %>% 
  ggplot2::ggsave(filename = "output/map_image/TimeSeriesCZ/1980to2015_CZmap_original_kanto.png", bg = "white")

base::rm(map_nameKanto, map_name, fileNameKanto, fileName, cz_map, map, tableName, tableNameKanto,
         czPath, mapPath, i, temp_fill, map_kanto,
         CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015,
         CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto, 
         CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto,)

