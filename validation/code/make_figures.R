# Figures of the replication: the map of between-delineation similarity, the similarity
# and diagnostic profile as functions of the cutoff, the map of non-contiguous
# municipalities, and the detail map of the commuting zone with the lowest mean
# containment. These are the counterparts of Figures 1, 3, 4 and 5 of the source paper.
#
# The maps are produced at both cutoff anchors, since the cutoff is carried in the file
# name and the choice between the two is itself a result.
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

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"), col_types = cols(code = col_character())) %>%
  filter(in_scope)
geometry <- st_read(boundary_shp, quiet = TRUE, options = "ENCODING=CP932") %>%
  st_make_valid() %>%
  mutate(code = sprintf("%05d", as.integer(as.character(JISCODE)))) %>%
  filter(code %in% scope$code) %>%
  st_transform(crs_equal_area) %>%
  select(code)
okinawa_codes <- scope$code[scope$block == "Okinawa main island"]
placed <- inset_okinawa(geometry, okinawa_codes)
geometry <- placed$layer
okinawa_frame <- placed$frame
edges <- read_csv(file.path(data_dir, "adjacency_edges.csv"), col_types = cols(.default = col_character()))

map_theme <- theme_void(base_size = 11) +
  theme(legend.position = "right", legend.key.height = unit(1.1, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# On a map of Japan the land runs diagonally, so the legend fits inside the frame at the
# lower right and the map itself can then use the full width of the canvas.
national_map_theme <- theme_void(base_size = 20) +
  theme(legend.position = "inside", legend.position.inside = c(0.84, 0.22),
        legend.key.height = unit(1.3, "cm"), legend.key.width = unit(0.7, "cm"),
        legend.background = element_blank(),
        plot.margin = margin(2, 2, 2, 2),
        plot.background = element_rect(fill = "white", colour = NA))
line_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = NA))

similarity_by_municipality <- read_csv(file.path(output_dir, "similarity_by_municipality.csv"),
                                       col_types = cols(code = col_character()))
containment_by_zone <- read_csv(file.path(output_dir, "containment_by_zone.csv"), show_col_types = FALSE)
containment_by_municipality <- read_csv(file.path(output_dir, "containment_by_municipality.csv"),
                                        col_types = cols(code = col_character(), .default = col_guess()))
detached <- read_csv(file.path(output_dir, "noncontiguous_municipalities.csv"),
                     col_types = cols(code = col_character()))
sweep <- read_csv(file.path(output_dir, "cutoff_sweep.csv"), show_col_types = FALSE)

# Romanized names for the municipalities the detail map labels. The boundary layer
# carries Japanese names only, and figure labels are written in English throughout the
# project. Anything absent falls back to its municipality code.
romanized <- c(
  "13211" = "Kodaira", "13213" = "Higashimurayama", "13221" = "Kiyose",
  "13222" = "Higashikurume", "13229" = "Nishitokyo"
)

cut_label <- function(cutoff) format(cutoff, nsmall = 3)

# Similarity as a function of the cutoff applied to the later year, the counterpart of
# Figure 4. Both anchors appear on the same panel, one line each.
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
         width = 7, height = 4.5, dpi = 300, bg = "white")
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
         width = 7, height = 4.5, dpi = 300, bg = "white")

  ggplot(panel, aes(x = cutoff_later, y = share_of_labour_force_contained)) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = cutoff_anchors, colour = "grey60", linewidth = 0.3) +
    labs(x = sprintf("Cutoff applied to the %d delineation", later),
         y = "Share of the resident labour force working inside its own zone") +
    line_theme
  ggsave(file.path(figure_dir, sprintf("containment_vs_cutoff_%d.png", later)),
         width = 7, height = 4.5, dpi = 300, bg = "white")

  ggplot(panel, aes(x = cutoff_later, y = noncontiguous_zones)) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = cutoff_anchors, colour = "grey60", linewidth = 0.3) +
    labs(x = sprintf("Cutoff applied to the %d delineation", later),
         y = "Number of commuting zones holding a detached municipality") +
    line_theme
  ggsave(file.path(figure_dir, sprintf("noncontiguous_zones_vs_cutoff_%d.png", later)),
         width = 7, height = 4.5, dpi = 300, bg = "white")
}

for (headline_cutoff in cutoff_anchors) {
  zones <- read_csv(file.path(derived_dir, sprintf("zones_%d_cut%s.csv", headline_later,
                                                   cut_label(headline_cutoff))),
                    col_types = cols(code = col_character(), zone = col_integer()))

  # Map of between-delineation similarity, the counterpart of Figure 1.
  headline <- similarity_by_municipality %>%
    filter(earlier_year == headline_earlier, later_year == headline_later,
           abs(cutoff - headline_cutoff) < 1e-9)
  ggplot(geometry %>% inner_join(headline, by = "code")) +
    geom_sf(aes(fill = similarity), colour = NA) +
    geom_sf(data = okinawa_frame, fill = NA, colour = "grey55", linewidth = 0.2) +
    scale_fill_viridis_c(name = "Jaccard\nsimilarity", limits = c(0, 1), option = "rocket", direction = 1) +
    map_theme
  ggsave(file.path(figure_dir, sprintf("similarity_map_%d_%d_cut%s.png", headline_earlier, headline_later,
                                       cut_label(headline_cutoff))),
         width = 7.5, height = 7.5, dpi = 300, bg = "white")

  # Map of the non-contiguous municipalities and the zones they are assigned to, the
  # counterpart of Figure 5.
  detached_here <- detached %>%
    filter(year == headline_later, abs(cutoff - headline_cutoff) < 1e-9)
  if (nrow(detached_here) > 0) {
    affected <- geometry %>%
      inner_join(zones, by = "code") %>%
      filter(zone %in% detached_here$zone) %>%
      mutate(role = ifelse(code %in% detached_here$code, "detached municipality",
                           "rest of its commuting zone"))
    ggplot() +
      geom_sf(data = geometry, fill = "grey95", colour = "grey85", linewidth = 0.05) +
      geom_sf(data = affected, aes(fill = role), colour = "grey40", linewidth = 0.1) +
      geom_sf(data = okinawa_frame, fill = NA, colour = "grey55", linewidth = 0.2) +
      scale_fill_manual(name = NULL, values = c("detached municipality" = "#7b1d3a",
                                                "rest of its commuting zone" = "#2c6fa8")) +
      national_map_theme
    ggsave(file.path(figure_dir, sprintf("noncontiguous_municipalities_%d_cut%s.png", headline_later,
                                         cut_label(headline_cutoff))),
           width = 7.5, height = 7.5, dpi = 300, bg = "white")
  }

  # Detail map of the commuting zone with the lowest mean containment, the counterpart of
  # Figure 3. Every municipality drawn is shaded by its own containment, the extent is the
  # zone together with every commuting zone adjacent to it drawn whole, zone boundaries are
  # drawn heavy and labelled by zone number, and the municipalities of the zone itself are
  # named. The source paper lays its equivalent over a Mapbox basemap; that needs an access
  # token and a network call at render time, so the surrounding zones supply the context
  # here instead.
  worst <- containment_by_zone %>%
    filter(year == headline_later, abs(cutoff - headline_cutoff) < 1e-9) %>%
    slice_min(mean_contained, n = 1)
  worst_units <- zones %>% filter(zone == worst$zone)

  # The extent is a set of whole commuting zones, not a distance buffer: the zone itself
  # plus every zone holding a municipality adjacent to one of its members.
  adjacent_codes <- unique(c(
    edges$code_j[edges$code_i %in% worst_units$code],
    edges$code_i[edges$code_j %in% worst_units$code]
  ))
  shown_zones <- unique(c(worst$zone, zones$zone[zones$code %in% adjacent_codes]))

  detail <- geometry %>%
    inner_join(zones, by = "code") %>%
    filter(zone %in% shown_zones) %>%
    left_join(containment_by_municipality %>%
                filter(year == headline_later, abs(cutoff - headline_cutoff) < 1e-9) %>%
                select(code, contained), by = "code") %>%
    mutate(containment_class = cut(contained, breaks = c(0, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                                   labels = c("below 0.4", "0.4 to 0.5", "0.5 to 0.6", "0.6 to 0.7",
                                              "0.7 to 0.8", "0.8 to 0.9", "0.9 to 1.0"),
                                   include.lowest = TRUE))
  zone_outlines <- detail %>% group_by(zone) %>% summarise(.groups = "drop") %>% st_make_valid()
  zone_labels <- suppressWarnings(st_point_on_surface(zone_outlines))
  # One municipality is named, the least contained of the zone, as the source paper names
  # the single county its text argues about. Naming all five crowds a cluster this small.
  named <- detail %>%
    filter(zone == worst$zone) %>%
    slice_min(contained, n = 1) %>%
    mutate(label = ifelse(code %in% names(romanized), romanized[code], code))
  named <- suppressWarnings(st_point_on_surface(named))

  ggplot() +
    geom_sf(data = detail, aes(fill = containment_class), colour = "grey70", linewidth = 0.1) +
    geom_sf(data = zone_outlines, fill = NA, colour = "black", linewidth = 0.7) +
    geom_sf_text(data = zone_labels, aes(label = zone), size = 6.6, fontface = "bold", colour = "white") +
    geom_sf_text(data = zone_labels, aes(label = zone), size = 6.0, fontface = "bold", colour = "grey15") +
    ggrepel::geom_text_repel(data = named, aes(geometry = geometry, label = label),
                             stat = "sf_coordinates", size = 4.6, colour = "grey10",
                             nudge_x = -95000, nudge_y = -55000, hjust = 1,
                             min.segment.length = 0, segment.colour = "grey40", segment.size = 0.3,
                             box.padding = 0.3, max.overlaps = Inf, seed = 20260826) +
    scale_fill_brewer(name = "Share of residents working\ninside their own commuting zone",
                      palette = "BuPu", drop = FALSE) +
    map_theme
  ggsave(file.path(figure_dir, sprintf("lowest_containment_zone_%d_cut%s.png", headline_later,
                                       cut_label(headline_cutoff))),
         width = 9.5, height = 5.6, dpi = 300, bg = "white")

  message("cutoff ", cut_label(headline_cutoff), ": lowest mean containment in ", headline_later,
          " is zone ", worst$zone, " at ", round(worst$mean_contained, 3),
          " over ", worst$municipalities, " municipalities; detail map shows ",
          length(shown_zones), " zones and ", nrow(detail), " municipalities")
}
