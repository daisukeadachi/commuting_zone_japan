# The Tokyo metropolitan area under the two delineations, one map each.
#
# The delineation built on the flows of the people who work alongside housework is about
# twice as fine as the baseline, and the two partitions agree on barely half the
# municipalities. Where that shows most is the Tokyo metropolitan area: the baseline joins
# the whole commuter belt to the capital, while this group's own flows leave much of the
# belt in zones of its own, because these people mostly work where they live.
#
# The two maps share a frame and a colouring rule, so that the reader compares the zone
# boundaries rather than the colours. Adjacent zones are given different shades by
# colouring on the zone label modulo the palette length; the shades themselves mean
# nothing.
#
# Writes two files under validation/output/figures, one per delineation. Neither carries a
# title, as with every figure here; the file name identifies it.

suppressMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
})
source("validation/code/config.R")
sf_use_s2(FALSE)

stopifnot(sample_definition == "WORK_MAIN", denominator == "reported")
map_year <- 2015L
map_cutoff <- baseline_cutoff

# Prefectures of the Kanto region, plus Yamanashi, which the Tokyo zone reaches into.
frame_prefectures <- c("08", "09", "10", "11", "12", "13", "14", "19")

figure_dir <- figure_path()
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"),
                  col_types = cols(code = col_character())) %>% filter(in_scope)
frame_codes <- scope$code[substr(scope$code, 1, 2) %in% frame_prefectures]
geometry <- read_boundaries(scope$code)

read_zones <- function(sample) {
  old <- sample_definition
  assign("sample_definition", sample, envir = globalenv())
  path <- zone_path(map_year, map_cutoff, "constrained")
  assign("sample_definition", old, envir = globalenv())
  read_csv(path, col_types = cols(code = col_character(), zone = col_integer()))
}

delineations <- list(
  mainly_working = read_zones("WORK_MAIN"),
  alongside_housework = read_zones("SIDE_WORK")
)

# The frame is the bounding box of the Kanto municipalities, so both maps show the same
# ground however differently they cut it.
frame <- st_bbox(geometry[geometry$code %in% frame_codes, ])

# Eight pale shades, enough that neighbouring zones rarely share one. The value carries no
# meaning, so a qualitative palette is right and no legend is drawn.
palette <- RColorBrewer::brewer.pal(8, "Set2")

for (name in names(delineations)) {
  zones <- delineations[[name]]
  layer <- geometry %>%
    left_join(zones, by = "code") %>%
    filter(code %in% frame_codes, !is.na(zone)) %>%
    mutate(shade = factor(zone %% length(palette)))
  outlines <- layer %>% group_by(zone) %>% summarise(.groups = "drop") %>% st_make_valid()

  ggplot() +
    geom_sf(data = layer, aes(fill = shade), colour = "grey75", linewidth = 0.12) +
    geom_sf(data = outlines, fill = NA, colour = "grey10", linewidth = 0.55) +
    scale_fill_manual(values = palette, guide = "none") +
    coord_sf(xlim = frame[c("xmin", "xmax")], ylim = frame[c("ymin", "ymax")], expand = FALSE) +
    theme_void(base_size = 14) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(2, 2, 2, 2))
  ggsave(file.path(figure_dir, sprintf("kanto_zones_%s_%d_cut%s.png", name, map_year,
                                       cut_label(map_cutoff))),
         width = 6.4, height = 6.0, dpi = 300, bg = "white")

  message(name, ": ", n_distinct(layer$zone), " zones over ", nrow(layer),
          " municipalities in the frame")
}
