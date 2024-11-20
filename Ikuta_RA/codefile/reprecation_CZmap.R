library(tidyverse)
library(sf)
library(RColorBrewer)

lineMatrix = base::rbind(c(138, 45), c(138, 40), c(130, 37))
OkinawaLine <- st_linestring(lineMatrix) %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4612)


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
  sf::st_transform(4612)

CZ_Okinawa <- CZ_2005.sf %>% 
  dplyr::filter(JISCODE %in% (47000:47999)) 
  sf::st_set_geometry(st_geometry(CZ_2005.sf %>% dplyr::filter(JISCODE %in% (47000:47999))) + c(17, )) %>% 
  sf::st_set_crs(4612)
# CZ_Okinawa <- CZ_Okinawa %>% 
#   sf::st_set_geometry((st_geometry(CZ_Okinawa) - st_centroid(CZ_Okinawa %>% st_geometry())) * 2 +  st_centroid(CZ_Okinawa %>% st_geometry())) %>% 

# custom_palette <- colorRampPalette(brewer.pal(8, "Set2"))(base::max(CZ_2005$cluster) + 1)
  
CZ_2005.sf %>%
  # dplyr::filter(JISCODE != 13421, !(JISCODE %in% (47000:47999))) %>% 
  # dplyr::bind_rows(CZ_Okinawa) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = fill), linewidth = 0.01, color = "gray") +
  # ggplot2::scale_fill_manual(values = custom_palette) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_sf(data = OkinawaLine) +
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
