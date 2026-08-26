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
land <- st_union(geometry)

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
sweep_theme <- theme_bw(base_size = 16) +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
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

# Colour scales follow the source paper. The similarity map bins the score at its breaks
# and shades the classes with a five-class BuPu ramp. The containment detail map uses the
# same ramp over its own six classes; the Japanese data reaches below the floor of those
# classes, so one lighter step is prepended.
similarity_breaks <- c(0, 0.03, 0.25, 0.5, 0.75, 1)
similarity_labels <- c("0", "0.03 to 0.25", "0.25 to 0.5", "0.5 to 0.75", "0.75 to 1")
containment_palette <- c("#F7FCFD", RColorBrewer::brewer.pal(6, "BuPu"))
names(containment_palette) <- c("below 0.4", "0.4 to 0.5", "0.5 to 0.6", "0.6 to 0.7",
                                "0.7 to 0.8", "0.8 to 0.9", "0.9 to 1.0")

# Similarity as a function of the cutoff applied to the later year, the counterpart of
# Figure 4. The presentation follows the source paper: the mean with a band one standard
# deviation wide, a dashed line at the anchor and a dotted line at the maximum.
for (pair in baseline_pairs) {
  for (anchor_value in cutoff_anchors) {
    panel <- sweep %>%
      filter(earlier_year == pair[1], later_year == pair[2], abs(anchor - anchor_value) < 1e-9)
    best <- panel %>% slice_max(mean_similarity, n = 1)
    top <- max(panel$mean_similarity + panel$sd_similarity)
    bottom <- min(panel$mean_similarity - panel$sd_similarity)
    ggplot(panel, aes(x = cutoff_later, y = mean_similarity)) +
      geom_ribbon(aes(ymin = mean_similarity - sd_similarity, ymax = mean_similarity + sd_similarity),
                  alpha = 0.2) +
      geom_line() +
      geom_vline(xintercept = anchor_value, linetype = "dashed") +
      annotate("text", x = anchor_value + 0.0016, y = bottom + 0.80 * (top - bottom), angle = 270,
               size = 4.6, label = sprintf("Earlier year held at %s", cut_label(anchor_value))) +
      geom_vline(xintercept = best$cutoff_later, linetype = "dotted") +
      annotate("text", x = best$cutoff_later - 0.0016, y = bottom + 0.55 * (top - bottom), angle = 270,
               size = 4.6, label = sprintf("Maximum at %s", cut_label(best$cutoff_later))) +
      labs(x = "Cutoff value", y = "Mean Jaccard similarity") +
      sweep_theme
    ggsave(file.path(figure_dir, sprintf("similarity_vs_cutoff_%d_%d_anchor%s.png",
                                         pair[1], pair[2], cut_label(anchor_value))),
           width = 8, height = 5, dpi = 300, bg = "white")
  }
}

# The diagnostic profile over the same range, so that what a move in the cutoff trades
# away is visible alongside the similarity.
profile <- sweep %>% distinct(later_year, cutoff_later, commuting_zones,
                              single_municipality_zones, noncontiguous_zones,
                              share_of_labour_force_contained)
profile_panel <- function(panel, later, y, y_label, file) {
  ggplot(panel, aes(x = cutoff_later, y = .data[[y]])) +
    geom_line() +
    geom_vline(xintercept = 0.977, linetype = "dashed") +
    labs(x = "Cutoff value", y = y_label) +
    sweep_theme
  ggsave(file.path(figure_dir, sprintf(file, later)), width = 8, height = 5, dpi = 300, bg = "white")
}
for (later in unique(profile$later_year)) {
  panel <- profile %>% filter(later_year == later)
  profile_panel(panel, later, "commuting_zones", "Number of commuting zones",
                "zone_count_vs_cutoff_%d.png")
  profile_panel(panel, later, "share_of_labour_force_contained",
                "Share of the resident labour force working inside its own zone",
                "containment_vs_cutoff_%d.png")
  profile_panel(panel, later, "noncontiguous_zones",
                "Commuting zones holding a detached municipality",
                "noncontiguous_zones_vs_cutoff_%d.png")
}

for (headline_cutoff in cutoff_anchors) {
  zones <- read_csv(file.path(derived_dir, sprintf("zones_%d_cut%s.csv", headline_later,
                                                   cut_label(headline_cutoff))),
                    col_types = cols(code = col_character(), zone = col_integer()))

  # Map of between-delineation similarity, the counterpart of Figure 1.
  headline <- similarity_by_municipality %>%
    filter(earlier_year == headline_earlier, later_year == headline_later,
           abs(cutoff - headline_cutoff) < 1e-9)
  mapped <- geometry %>%
    inner_join(headline, by = "code") %>%
    mutate(similarity_class = cut(similarity, breaks = similarity_breaks,
                                  labels = similarity_labels, include.lowest = TRUE))
  ggplot(mapped) +
    geom_sf(aes(fill = similarity_class, colour = similarity_class), linewidth = 0.05) +
    geom_sf(data = okinawa_frame, fill = NA, colour = "grey55", linewidth = 0.2) +
    scale_fill_brewer(palette = "BuPu", na.value = "white", drop = FALSE) +
    scale_colour_brewer(palette = "BuPu", na.value = "white", drop = FALSE, guide = "none") +
    guides(fill = guide_legend(title = "Similarity")) +
    national_map_theme
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
                           "its commuting zone"))
    ggplot() +
      geom_sf(data = land, fill = "grey35", colour = "grey20", linewidth = 0.15, alpha = 0.4) +
      geom_sf(data = affected, aes(fill = role), colour = "grey40", linewidth = 0.1) +
      geom_sf(data = okinawa_frame, fill = NA, colour = "grey55", linewidth = 0.2) +
      scale_fill_manual(name = NULL, values = c("detached municipality" = "maroon",
                                                "its commuting zone" = "blue")) +
      national_map_theme +
      theme(legend.position.inside = c(0.79, 0.15),
            legend.key.height = unit(0.7, "cm"), legend.key.width = unit(0.7, "cm"))
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
    scale_fill_manual(name = "Share of residents working\ninside their own commuting zone",
                      values = containment_palette, drop = FALSE) +
    theme_void(base_size = 16) +
    theme(legend.position = "right", legend.key.height = unit(0.9, "cm"),
          plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(figure_dir, sprintf("lowest_containment_zone_%d_cut%s.png", headline_later,
                                       cut_label(headline_cutoff))),
         width = 9.5, height = 5.6, dpi = 300, bg = "white")

  message("cutoff ", cut_label(headline_cutoff), ": lowest mean containment in ", headline_later,
          " is zone ", worst$zone, " at ", round(worst$mean_contained, 3),
          " over ", worst$municipalities, " municipalities; detail map shows ",
          length(shown_zones), " zones and ", nrow(detail), " municipalities")
}
