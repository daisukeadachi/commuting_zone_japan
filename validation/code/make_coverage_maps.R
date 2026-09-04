# Commuting zones and Urban Employment Areas over the whole country, one map each.
#
# The argument for delineating commuting zones rather than using the existing Japanese
# alternative is coverage: the Urban Employment Area starts from urban centres and merges
# outwards, so a municipality far from any centre belongs to nothing, while a commuting
# zone delineation partitions the country. That difference is a fact about the two maps
# and is shown as one.
#
# Both panels are drawn on the same municipalities and the same frame, so the grey in the
# second is exactly what the first covers and the second does not. The year is 2000, the
# year the coverage of the Urban Employment Area is usually quoted for.
#
# Writes two files under validation/output/figures. Neither carries a title, as with every
# figure here; the file name identifies it.

suppressMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})
source("validation/code/config.R")
sf_use_s2(FALSE)

stopifnot(sample_definition == "WORK_MAIN", denominator == "reported",
          code_universe == "harmonized")
map_year <- 2000L
map_cutoff <- baseline_cutoff

figure_dir <- figure_path()
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

scope <- read_scope() %>% filter(in_scope)
geometry <- read_boundaries(scope$code)
placed <- inset_okinawa(geometry, scope$code[scope$block == "Okinawa main island"])
geometry <- placed$layer
okinawa_frame <- placed$frame

zones <- read_csv(zone_path(map_year, map_cutoff, "constrained"),
                  col_types = cols(code = col_character(), zone = col_integer()))

# The Urban Employment Area membership of the same year, read the way make_core.R reads
# it: a municipality belongs to at most one area, as a central city or as a suburb.
uea_dir <- file.path(data_dir, "uea")
uea_files <- c("MEA2000_Rev07.csv", "MEA2000C_Rev07.csv",
               "McEA2000_Rev07.csv", "McEA2000C_Rev07.csv")
read_cp932 <- function(path) {
  suppressWarnings(read_csv(path, locale = locale(encoding = "CP932"), show_col_types = FALSE,
                            name_repair = "unique"))
}
pad <- function(x) sprintf("%05d", as.integer(x))
crosswalk <- read_cp932(file.path(data_dir, "crosswalk",
                                  sprintf("codelist_%d1001and20151001.csv", map_year))) %>%
  transmute(from = pad(JISCODE1), to = pad(JISCODE2)) %>%
  distinct(from, .keep_all = TRUE)
harmonize <- function(codes) {
  out <- crosswalk$to[match(codes, crosswalk$from)]
  merge_tokyo_wards(ifelse(is.na(out), codes, out))
}

suburbs <- bind_rows(lapply(uea_files[c(1, 3)], function(f) {
  raw <- read_cp932(file.path(uea_dir, f)) %>% distinct()
  columns <- names(raw)[grepl("^suburb[0-9]*$", names(raw), ignore.case = TRUE)]
  raw %>%
    select(area = 1, all_of(columns)) %>%
    mutate(across(all_of(columns), as.character)) %>%
    pivot_longer(all_of(columns), values_to = "code") %>%
    filter(!is.na(code), suppressWarnings(!is.na(as.integer(code)))) %>%
    transmute(area = pad(area), code = pad(code))
}))
centres <- bind_rows(lapply(uea_files[c(2, 4)], function(f) {
  read_cp932(file.path(uea_dir, f)) %>%
    distinct() %>%
    select(area = 1, code = 4) %>%
    transmute(area = pad(area), code = pad(code))
}))
areas <- bind_rows(centres, suburbs) %>%
  transmute(area = harmonize(area), code = harmonize(code)) %>%
  distinct(code, .keep_all = TRUE)

# Adjacent units are given different shades by colouring on the label modulo the palette
# length; the shades themselves mean nothing, and no legend is drawn. The palette's own
# grey is dropped, so that grey means one thing on these maps: outside every urban area.
palette <- RColorBrewer::brewer.pal(8, "Set2")[1:7]
outside <- "grey82"

# The device is given the aspect ratio of the ground it draws, so that the map fills it
# rather than sitting in a band of white.
frame <- st_bbox(geometry)
map_width <- 6.0
map_height <- map_width * as.numeric((frame["ymax"] - frame["ymin"]) /
                                     (frame["xmax"] - frame["xmin"]))

draw <- function(layer, outlines, name) {
  ggplot() +
    geom_sf(data = layer, aes(fill = shade), colour = "grey70", linewidth = 0.05) +
    geom_sf(data = outlines, fill = NA, colour = "grey15", linewidth = 0.25) +
    geom_sf(data = okinawa_frame, fill = NA, colour = "grey55", linewidth = 0.2) +
    scale_fill_manual(values = c(setNames(palette, as.character(seq_along(palette) - 1)),
                                 "outside" = outside),
                      guide = "none", na.value = outside) +
    theme_void(base_size = 11) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(2, 2, 2, 2))
  ggsave(file.path(figure_dir, name), width = map_width, height = map_height,
         dpi = 300, bg = "white")
}

cz_layer <- geometry %>%
  left_join(zones, by = "code") %>%
  mutate(shade = as.character(zone %% length(palette)))
cz_outlines <- cz_layer %>%
  filter(!is.na(zone)) %>%
  group_by(zone) %>%
  summarise(.groups = "drop") %>%
  st_make_valid()
draw(cz_layer, cz_outlines, sprintf("coverage_commuting_zones_%d_cut%s.png", map_year,
                                    cut_label(map_cutoff)))

uea_layer <- geometry %>%
  left_join(areas, by = "code") %>%
  mutate(shade = ifelse(is.na(area), "outside",
                        as.character(as.integer(factor(area)) %% length(palette))))
uea_outlines <- uea_layer %>%
  filter(!is.na(area)) %>%
  group_by(area) %>%
  summarise(.groups = "drop") %>%
  st_make_valid()
draw(uea_layer, uea_outlines, sprintf("coverage_urban_employment_areas_%d.png", map_year))

message(sprintf("%d: %d municipalities, %d commuting zones; %d municipalities in %d urban areas",
                map_year, nrow(geometry), n_distinct(zones$zone),
                sum(!is.na(uea_layer$area)), n_distinct(areas$area)))
