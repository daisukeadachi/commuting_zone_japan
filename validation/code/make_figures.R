# Figures of the replication: the map of between-delineation similarity, the similarity
# and diagnostic profile as functions of the cutoff, the map of non-contiguous
# municipalities, and the detail map of the commuting zone with the lowest mean
# containment. These are the counterparts of Figures 1, 3, 4 and 5 of the source paper.
#
# Each figure is written to its own file under validation/output/figures and carries no
# title; the file name identifies it and the caption is written where the figure is used.

suppressMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(ggplot2)
})
source("validation/code/config.R")
sf_use_s2(FALSE)

figure_dir <- file.path(output_dir, "figures")
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

headline_earlier <- 2010
headline_later <- 2020
headline_cutoff <- 0.980

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"), col_types = cols(code = col_character())) %>%
  filter(in_scope)
geometry <- st_read(boundary_shp, quiet = TRUE, options = "ENCODING=CP932") %>%
  st_make_valid() %>%
  mutate(code = sprintf("%05d", as.integer(as.character(JISCODE)))) %>%
  filter(code %in% scope$code) %>%
  st_transform(crs_equal_area) %>%
  select(code)

map_theme <- theme_void(base_size = 11) +
  theme(legend.position = "right", legend.key.height = unit(1.1, "cm"))
line_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

# Map of between-delineation similarity, the counterpart of Figure 1.
similarity_by_municipality <- read_csv(file.path(output_dir, "similarity_by_municipality.csv"),
                                       col_types = cols(code = col_character()))
headline <- similarity_by_municipality %>%
  filter(earlier_year == headline_earlier, later_year == headline_later,
         abs(cutoff - headline_cutoff) < 1e-9)
similarity_map <- geometry %>% inner_join(headline, by = "code")
ggplot(similarity_map) +
  geom_sf(aes(fill = similarity), colour = NA) +
  scale_fill_viridis_c(name = "Jaccard\nsimilarity", limits = c(0, 1), option = "rocket", direction = 1) +
  map_theme
ggsave(file.path(figure_dir, sprintf("similarity_map_%d_%d_cut%s.png", headline_earlier, headline_later,
                                     format(headline_cutoff, nsmall = 3))),
       width = 7, height = 8, dpi = 300)

# Similarity as a function of the cutoff applied to the later year, the counterpart of
# Figure 4.
sweep <- read_csv(file.path(output_dir, "cutoff_sweep.csv"), show_col_types = FALSE)
for (pair in baseline_pairs) {
  panel <- sweep %>% filter(earlier_year == pair[1], later_year == pair[2])
  ggplot(panel, aes(x = cutoff_later, y = mean_similarity,
                    colour = factor(anchor), linetype = factor(anchor))) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = cutoff_anchors, colour = "grey60", linewidth = 0.3) +
    scale_colour_grey(name = "Cutoff holding the earlier year", start = 0.1, end = 0.55) +
    scale_linetype_discrete(name = "Cutoff holding the earlier year") +
    labs(x = sprintf("Cutoff applied to the %d delineation", pair[2]),
         y = "Mean Jaccard similarity across municipalities") +
    line_theme
  ggsave(file.path(figure_dir, sprintf("similarity_vs_cutoff_%d_%d.png", pair[1], pair[2])),
         width = 7, height = 4.5, dpi = 300)
}

# The diagnostic profile over the same range, so that what a move in the cutoff trades
# away is visible alongside the similarity.
profile <- sweep %>% distinct(later_year, cutoff_later, commuting_zones,
                              single_municipality_zones, noncontiguous_zones,
                              share_of_labour_force_contained)
for (later in unique(profile$later_year)) {
  panel <- profile %>% filter(later_year == later)
  ggplot(panel, aes(x = cutoff_later, y = commuting_zones)) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = cutoff_anchors, colour = "grey60", linewidth = 0.3) +
    labs(x = sprintf("Cutoff applied to the %d delineation", later), y = "Number of commuting zones") +
    line_theme
  ggsave(file.path(figure_dir, sprintf("zone_count_vs_cutoff_%d.png", later)),
         width = 7, height = 4.5, dpi = 300)

  ggplot(panel, aes(x = cutoff_later, y = share_of_labour_force_contained)) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = cutoff_anchors, colour = "grey60", linewidth = 0.3) +
    labs(x = sprintf("Cutoff applied to the %d delineation", later),
         y = "Share of the resident labour force working inside its own zone") +
    line_theme
  ggsave(file.path(figure_dir, sprintf("containment_vs_cutoff_%d.png", later)),
         width = 7, height = 4.5, dpi = 300)

  ggplot(panel, aes(x = cutoff_later, y = noncontiguous_zones)) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = cutoff_anchors, colour = "grey60", linewidth = 0.3) +
    labs(x = sprintf("Cutoff applied to the %d delineation", later),
         y = "Number of commuting zones holding a detached municipality") +
    line_theme
  ggsave(file.path(figure_dir, sprintf("noncontiguous_zones_vs_cutoff_%d.png", later)),
         width = 7, height = 4.5, dpi = 300)
}

# Map of the non-contiguous municipalities and the zones they are assigned to, the
# counterpart of Figure 5.
detached <- read_csv(file.path(output_dir, "noncontiguous_municipalities.csv"),
                     col_types = cols(code = col_character()))
zones <- read_csv(file.path(derived_dir, sprintf("zones_%d_cut%s.csv", headline_later,
                                                 format(headline_cutoff, nsmall = 3))),
                  col_types = cols(code = col_character(), zone = col_integer()))
detached_here <- detached %>% filter(year == headline_later, abs(cutoff - headline_cutoff) < 1e-9)
if (nrow(detached_here) > 0) {
  affected <- geometry %>%
    inner_join(zones, by = "code") %>%
    filter(zone %in% detached_here$zone) %>%
    mutate(role = ifelse(code %in% detached_here$code, "detached municipality", "rest of its commuting zone"))
  ggplot() +
    geom_sf(data = geometry, fill = "grey95", colour = "grey85", linewidth = 0.05) +
    geom_sf(data = affected, aes(fill = role), colour = "grey40", linewidth = 0.1) +
    scale_fill_manual(name = NULL, values = c("detached municipality" = "#7b1d3a",
                                              "rest of its commuting zone" = "#2c6fa8")) +
    map_theme
  ggsave(file.path(figure_dir, sprintf("noncontiguous_municipalities_%d_cut%s.png", headline_later,
                                       format(headline_cutoff, nsmall = 3))),
         width = 7, height = 8, dpi = 300)
}

# Detail map of the commuting zone with the lowest mean containment, the counterpart of
# Figure 3.
containment_by_zone <- read_csv(file.path(output_dir, "containment_by_zone.csv"), show_col_types = FALSE)
worst <- containment_by_zone %>%
  filter(year == headline_later, abs(cutoff - headline_cutoff) < 1e-9) %>%
  slice_min(mean_contained, n = 1)
worst_units <- zones %>% filter(zone == worst$zone)
worst_geometry <- geometry %>% inner_join(worst_units, by = "code")
frame <- st_bbox(st_buffer(st_union(worst_geometry), 40000))
surroundings <- geometry %>%
  inner_join(zones, by = "code") %>%
  st_filter(st_as_sfc(frame))
ggplot() +
  geom_sf(data = surroundings, fill = "grey95", colour = "grey80", linewidth = 0.15) +
  geom_sf(data = worst_geometry, fill = "#2c6fa8", colour = "white", linewidth = 0.25) +
  coord_sf(xlim = frame[c("xmin", "xmax")], ylim = frame[c("ymin", "ymax")]) +
  map_theme
ggsave(file.path(figure_dir, sprintf("lowest_containment_zone_%d_cut%s.png", headline_later,
                                     format(headline_cutoff, nsmall = 3))),
       width = 7, height = 6, dpi = 300)

message("lowest mean containment in ", headline_later, ": zone ", worst$zone,
        " at ", round(worst$mean_contained, 3), " over ", worst$municipalities, " municipalities")
