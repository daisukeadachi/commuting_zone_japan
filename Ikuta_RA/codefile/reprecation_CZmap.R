library(tidyverse)
library(sf)
#DropboxからDLしたデータはすべてdataフォルダに格納

CZ_data <- readr::read_csv("data/2005_original_small-0.001.csv") %>% 
  dplyr::rename(JISCODE = i)

muni_map <- sf::read_sf("data/mmm20051001/mmm20051001.shp", options = "ENCODING=CP932") %>% 
  dplyr::left_join(CZ_data, by = "JISCODE")                                                  


muni_map %>% 
  dplyr::mutate(cluster = as.factor(cluster)) %>% 
  dplyr::filter(JISCODE %in% (5000:5999)) %>%
  dplyr::select(cluster) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(aes(fill = cluster)) +
  ggplot2::theme_bw() +
  # ggplot2::scale_fill_brewer(type = "qua") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(title = "Commuting Zone")




