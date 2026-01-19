pacman::p_load(
  tidyverse,
  sf,
  patchwork,
  spdep,
  RColorBrewer,
  furrr,
  parallel
)

rm(list = ls())

# helper for assigning colors to grouped polygons
source("codefile/color_assignment_impl.R")

n_core <- 10
plan(multicore, workers = n_core)


"%not.in%" <- Negate("%in%")
colors <- RColorBrewer::brewer.pal(5, "Set2")
# c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
# limitation range

lim <- list(x = c(129.3, 142.3), y = c(31.2, 42))
width <- .1
outdir <- "output/map_image/CZ2015/"


# directory to persist signature->color maps across runs
color_map_dir <- "output/color_map"

czlist.all <- list.files(path = "output/clustered/original", full.names = TRUE, recursive = TRUE) |>
  purrr::set_names(~ stringr::str_remove(basename(.x), "\\.csv$"))
year <- seq(1980, 2015, 5) |> as.character()

IMGDIR <- "output/map_image/tree-height/original/"
iwalk(czlist.all, ~ {
  folder <- dirname(.x) |> stringr::str_remove("output/clustered/original/") 
  dir.create(paste0(IMGDIR, folder), recursive = TRUE, showWarnings = FALSE)
})

#### map making ####
walk(year, function(YEAR) {
  muni.path <- stringr::str_glue("mapdata/mmm{YEAR}1001/mmm{YEAR}1001.shp")
  muni.sf <- sf::read_sf(muni.path, options = "ENCODING=CP932") %>%
    # 北方領土･小笠原諸島は解釈が難しいので、地図には出さない
    dplyr::filter(
      JISCODE %not.in% c(1695, 1696, 1698, 13421)
    ) %>%
    dplyr::select(-NO, -DATE) %>%
    sf::st_transform(4612)
  czlist <- czlist.all %>% stringr::str_subset(YEAR) %>%
    purrr::set_names(~ stringr::str_remove(basename(.x), "\\.csv$"))
  cli::cli_alert_info("Processing year: {YEAR}")
  future_iwalk(czlist, ~ {
    folder <- dirname(.x) |> stringr::str_remove("output/clustered/original/")
    ## map data ##################################################################
    CZ.sf <- muni.sf %>%
      dplyr::left_join(readr::read_csv(.x), by = c("JISCODE" = "i")) %>%
      move_Hokaido()
    ## color assignment ##########################################################)
    EdoCenter <- which(CZ.sf$JISCODE == 13101)
    Gohunai <- CZ.sf$cluster[EdoCenter]
    CZ_color <- assign_group_colors(CZ.sf, "cluster", colors = colors, fixed = list(value = Gohunai, color = RColorBrewer::brewer.pal(6, "Set2")[6]))
    CZ.sf <- dplyr::left_join(CZ.sf, CZ_color, by = "cluster") %>%
      dplyr::select(geometry, color)
    # persist and save CZ signature->color map for next iteration
    rm(CZ_color, EdoCenter, Gohunai)
    ## ggplot zone(for double) #################################################
    CZ.sf %>%
      make_basic_plot(
        lim_x = lim$x,
        lim_y = lim$y,
        HokkaidoLine = TRUE
      ) -> enl
    ggplot2::ggsave(enl,
      filename = paste0(IMGDIR, folder, "/", .y, ".png"),
      device = "png",
      bg = "white"
    )
  },.options = furrr::furrr_options(seed = TRUE),
  .progress = TRUE
  )
  cli::cli_alert_success("Finished year: {YEAR}")
})
