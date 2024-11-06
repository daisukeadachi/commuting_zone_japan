library(tidyverse)
library(sf)
#DropboxからDLしたデータはすべてdataフォルダに格納

CZ_2005 <- readr::read_csv("data/2005_original_small-0.001.csv") %>% 
  dplyr::rename(JISCODE = i)

muni_map <- sf::read_sf("data/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>% 
  dplyr::left_join(CZ_2005, by = "JISCODE")                                                  

temp <- muni_map %>% 
  dplyr::tibble() %>% 
  select(cluster, NAME) %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::slice_head(n = 1) %>% 
  dplyr::rename(rep = NAME)

muni_map %>% 
  dplyr::left_join(temp, by = "cluster") %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = rep)) +
  ggplot2::theme_bw() +
  # ggplot2::scale_fill_brewer(type = "qua") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(title = "Commuting Zone(2005)") -> CZmap_2005

muni_map %>% 
  dplyr::left_join(temp, by = "cluster") %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = rep)) +
  ggplot2::theme_bw() +
  # ggplot2::scale_fill_brewer(type = "qua") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(ylim = c(34, 37),
                    xlim = c(138, 141),
                    datum = NA) +
  ggplot2::labs(title = "関東地方・Commuting Zone(2005)") -> CZmap_2005_Kanto

ggsave(CZmap_2005, filename = "output/CZmap_2005.png")
ggsave(CZmap_2005_Kanto, filename = "output/CZmap_2005_Kanto.png")


