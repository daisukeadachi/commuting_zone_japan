library(tidyverse)
library(sf)

#2005
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
                   by = "cluster")


CZ_2005.sf %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = fill), linewidth = 0.01, color = "gray") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(datum = NA) +
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

CZ_2005.sf %>% 
  dplyr::filter(JISCODE %in% (12000:12999)) -> temp_chiba2
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = fill), linewidth = 0.01, color = "gray") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(title = "Commuting Zone(2005)") -> CZ_chiba
  ggplot2::ggsave(filename = "output/UEA&CAmap_2005rep.png", bg = "white")

  
temp <- read_csv("data/2005_original_small-0.001.csv")  %>% 
  dplyr::filter(i %in% (12000:12999)) 
temp2<- read_csv("output/2005_original.csv") %>% 
  dplyr::filter(i %in% (12000:12999)) %>% 
  left_join(temp, by = "i")

temp3 <- intersect(temp,temp2)

#harmonized (2015)
