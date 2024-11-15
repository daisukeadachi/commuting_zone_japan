library(tidyverse)
library(sf)
library(gridExtra)


#2005---------------------------------------------------------------------------
# 
# CZ_2005 <- readr::read_csv("output/2005_original.csv")
# 
# 
# CZ_2005.sf <- sf::read_sf("mapdata/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>% 
#   dplyr::left_join(CZ_2005, by = c("JISCODE" = "i"))
# CZ_2005.sf <- CZ_2005.sf %>% 
#   dplyr::left_join(CZ_2005.sf %>% 
#                      dplyr::tibble() %>% 
#                      dplyr::select(cluster, NAME) %>% 
#                      dplyr::group_by(cluster) %>% 
#                      dplyr::slice_head(n = 1) %>% 
#                      dplyr::rename(fill = NAME),
#                    by = "cluster")
# 
# 
# CZ_2005.sf %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(aes(fill = fill), linewidth = 0.01, color = "gray") +
#   ggplot2::theme_bw() +
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::coord_sf(datum = NA) +
#   ggplot2::labs(title = "CZ_alt") -> CZmap_2005
# 
# CZ_2005.sf %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(aes(fill = fill)) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(legend.position = "none") +
#   ggplot2::coord_sf(ylim = c(34, 37),
#                     xlim = c(138, 141),
#                     datum = NA) +
#   ggplot2::labs(title = "関東地方・Commuting Zone(2005)")+
#   ggplot2::theme(plot.title    = element_text(size = 10)) -> CZmap_2005_Kanto
# 
# ggplot2::ggsave(filename = "output/mapB.png", bg = "white")
# 

#harmonized (2015)--------------------------------------------------------------

mapdata <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") 
year <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)

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
  
  fileName <- base::paste0("output/map_image/CZ_", i, ".png")
  fileNameKanto <- base::paste0("output/map_image/CZ_", i, "_Kanto.png")
  # ggplot2::ggsave(map, filename = fileName, bg = "white")
  # ggplot2::ggsave(map_kanto, filename = fileNameKanto, bg = "white")
  map_name <- base::paste0("CZ_", i)
  base::assign(map_name, map)
  map_nameKanto <- base::paste0("CZ_", i, "_Kanto")
  base::assign(map_nameKanto, map_kanto)
  
}


gridExtra::grid.arrange(CZ_1980, CZ_1985, CZ_1990, CZ_1995, CZ_2000, CZ_2005, CZ_2010, CZ_2015, nrow = 3) %>% 
  ggplot2::ggsave(filename = "output/map_image/CZmap_1980to2015.png", bg = "white")

gridExtra::grid.arrange(CZ_1980_Kanto, CZ_1985_Kanto, CZ_1990_Kanto, CZ_1995_Kanto, 
                        CZ_2000_Kanto, CZ_2005_Kanto, CZ_2010_Kanto, CZ_2015_Kanto, nrow = 3) %>% 
  ggplot2::ggsave(filename = "output/map_image/CZmap_1980to2015_kanto.png", bg = "white")
