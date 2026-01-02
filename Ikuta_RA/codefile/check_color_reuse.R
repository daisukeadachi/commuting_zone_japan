library(tidyverse)
library(sf)
source("codefile/color_assignment.R")

"%not.in%" <- Negate("%in%")
muni.sf <- sf::read_sf("mapdata/mmm20151001/mmm20151001.shp", options = "ENCODING=CP932") %>%
  dplyr::filter(JISCODE %not.in% c(1695, 1696, 1698, 13421), JISCODE %in% (7000:23999)) %>%
  dplyr::select(-NO, -DATE) %>%
  sf::st_transform(4612)

# helper
get_cz_sf <- function(y){
  if (y == 2020) czPath  <- "data/addCZdata/2020_harmonized_small-0.001_tree_height-0.98.csv" else czPath <- paste0("output/", y, "_harmonized.csv")
  CZ.sf <- muni.sf %>% dplyr::left_join(readr::read_csv(czPath, show_col_types = FALSE), by = c("JISCODE" = "i"))
  return(CZ.sf)
}

cz80 <- get_cz_sf(1980)
map80 <- assign_group_colors(cz80, "cluster")
prev_map <- setNames(map80$color, map80$signature)
cz15 <- get_cz_sf(2015)
map15 <- assign_group_colors(cz15, "cluster", prev_colors = prev_map)

# compute reuse
matched <- intersect(map15$signature, names(prev_map))
reused <- sum(map15$signature %in% matched & map15$color == prev_map[map15$signature])
cat("Total groups 2015:", nrow(map15), "\n")
cat("Signatures present in 1980:", length(matched), "\n")
cat("Groups that reused color:", reused, "\n")

# write small sample of reused signatures/colors
if(length(matched)>0){
  sample_df <- map15 %>% dplyr::filter(signature %in% matched) %>% dplyr::slice_head(n=10)
  print(sample_df)
}
