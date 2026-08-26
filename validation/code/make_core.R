# Core block of the fit statistics and the map of split urban areas, the counterparts of
# the Core half of Table 2 and of Figure 6 of the source paper.
#
# The core set and the externally defined areas come from the Urban Employment Area, for
# the reasons set out in validation/notes/core_definition.md. The three measures that
# need only a core set are reported a second time on a mechanical rule that a
# municipality is a core when its densely inhabited district population reaches 50,000,
# with a second tier at 10,000. That rule uses no commuting data, so agreement between
# the two versions is evidence that the Core measures read urban structure rather than
# re-reading the flows the delineation was built from.
#
# There is no 1985 Urban Employment Area delineation, so that year carries the district
# rule only.
#
# Writes validation/output/core_table.csv, validation/output/core_area_splits.csv and
# the map of split areas under validation/output/figures.

suppressMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})
source("validation/code/config.R")
sf_use_s2(FALSE)

uea_dir <- file.path(data_dir, "uea")
figure_dir <- figure_path()
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

# File names differ by year in both the suffix and the capitalisation of the micropolitan
# prefix, so the mapping is written out rather than constructed.
uea_files <- list(
  "1980" = c("MEA80_Rev07.csv", "MEA80C_Rev07.csv", "McEA80_Rev07.csv", "McEA80C_Rev07.csv"),
  "1990" = c("MEA90_Rev07.csv", "MEA90C_Rev07.csv", "McEA90_Rev07.csv", "McEA90C_Rev07.csv"),
  "1995" = c("MEA95_Rev07.csv", "MEA95C_Rev07.csv", "McEA95_Rev07.csv", "McEA95C_Rev07.csv"),
  "2000" = c("MEA2000_Rev07.csv", "MEA2000C_Rev07.csv", "McEA2000_Rev07.csv", "McEA2000C_Rev07.csv"),
  "2005" = c("MEA2005.csv", "MEA2005C.csv", "McEA2005.csv", "McEA2005C.csv"),
  "2010" = c("MEA2010.csv", "MEA2010C.csv", "MCEA2010.csv", "MCEA2010C.csv"),
  "2015" = c("MEA2015.csv", "MEA2015C.csv", "MCEA2015.csv", "MCEA2015C.csv"),
  "2020" = c("MEA2020.csv", "MEA2020C.csv", "MCEA2020.csv", "MCEA2020C.csv")
)

# Towns incorporated as cities between the 2015 and 2020 censuses, without any boundary
# change; mapping the 2020 code back gives the 2015 code.
city_incorporations_2020 <- c("04216" = "04423", "40231" = "40305")

read_cp932 <- function(path) {
  suppressWarnings(read_csv(path, locale = locale(encoding = "CP932"), show_col_types = FALSE,
                            name_repair = "unique"))
}

pad <- function(x) sprintf("%05d", as.integer(x))

crosswalk <- function(year) {
  if (year >= 2015) return(NULL)
  path <- file.path(data_dir, "crosswalk", sprintf("codelist_%d1001and20151001.csv", year))
  read_cp932(path) %>%
    transmute(from = pad(JISCODE1), to = pad(JISCODE2)) %>%
    distinct(from, .keep_all = TRUE)
}

harmonize <- function(codes, year) {
  if (year == 2020) {
    return(ifelse(codes %in% names(city_incorporations_2020), city_incorporations_2020[codes], codes))
  }
  map <- crosswalk(year)
  if (is.null(map)) return(codes)
  out <- map$to[match(codes, map$from)]
  ifelse(is.na(out), codes, out)
}

read_uea <- function(year) {
  files <- uea_files[[as.character(year)]]
  suburb_files <- files[c(1, 3)]
  centre_files <- files[c(2, 4)]

  suburbs <- bind_rows(lapply(suburb_files, function(f) {
    raw <- read_cp932(file.path(uea_dir, f)) %>% distinct()
    columns <- names(raw)
    suburb_columns <- columns[grepl("^suburb[0-9]*$", columns, ignore.case = TRUE)]
    raw %>%
      select(area = 1, all_of(suburb_columns)) %>%
      mutate(across(all_of(suburb_columns), as.character)) %>%
      pivot_longer(all_of(suburb_columns), values_to = "code") %>%
      filter(!is.na(code), suppressWarnings(!is.na(as.integer(code)))) %>%
      transmute(area = pad(area), code = pad(code))
  }))

  centres <- bind_rows(lapply(centre_files, function(f) {
    read_cp932(file.path(uea_dir, f)) %>%
      distinct() %>%
      select(area = 1, code = 4) %>%
      transmute(area = pad(area), code = pad(code))
  }))

  bind_rows(
    centres %>% mutate(is_core = TRUE),
    suburbs %>% mutate(is_core = FALSE)
  ) %>%
    mutate(area = harmonize(area, year), code = harmonize(code, year)) %>%
    group_by(area, code) %>%
    summarise(is_core = any(is_core), .groups = "drop")
}

# The district table is on ward codes, so its ward rows are added together into the
# merged unit before the mechanical core rule reads it.
districts <- read_csv(file.path(data_dir, "did_municipality_harmonized.csv"),
                      col_types = cols(muni_code = col_character(), year = col_integer(),
                                       did_population = col_double(), .default = col_guess())) %>%
  mutate(muni_code = merge_tokyo_wards(muni_code)) %>%
  group_by(year, muni_code) %>%
  summarise(did_population = sum(did_population), .groups = "drop")

core_measures <- function(year, cutoff) {
  zones <- read_csv(zone_path(year, cutoff),
                    col_types = cols(code = col_character(), zone = col_integer()))
  flows <- read_commuting(year) %>% filter(i %in% zones$code, j %in% zones$code)
  zone_of <- setNames(zones$zone, zones$code)

  measure <- function(core_codes, label, areas = NULL) {
    is_core <- zones$code %in% core_codes
    core_by_zone <- tapply(is_core, zones$zone, any)
    residents <- flows %>%
      mutate(same = zone_of[i] == zone_of[j], into_core = same & j %in% core_codes) %>%
      group_by(code = i) %>%
      summarise(residents = sum(pop), into_core = sum(pop[into_core]), .groups = "drop")
    workforce <- flows %>%
      mutate(same = zone_of[i] == zone_of[j], from_core = same & i %in% core_codes) %>%
      group_by(code = j) %>%
      summarise(jobs = sum(pop), from_core = sum(pop[from_core]), .groups = "drop")
    joined <- zones %>%
      left_join(residents, by = "code") %>%
      left_join(workforce, by = "code")
    split_areas <- NA_integer_
    if (!is.null(areas)) {
      split_areas <- areas %>%
        mutate(zone = zone_of[code]) %>%
        filter(!is.na(zone)) %>%
        group_by(area) %>%
        summarise(zones = n_distinct(zone), .groups = "drop") %>%
        filter(zones > 1) %>%
        nrow()
    }
    tibble(
      year = year, cutoff = cutoff, core_definition = label,
      commuting_zones = length(core_by_zone),
      core_municipalities = sum(is_core),
      share_of_zones_with_a_core = mean(core_by_zone),
      share_working_in_a_core_of_own_zone = sum(joined$into_core) / sum(joined$residents),
      share_living_in_a_core_of_own_zone = sum(joined$from_core, na.rm = TRUE) / sum(joined$jobs, na.rm = TRUE),
      urban_areas_split_across_zones = split_areas
    )
  }

  rows <- list()
  if (!is.null(uea_files[[as.character(year)]])) {
    areas <- read_uea(year) %>% filter(code %in% zones$code)
    rows[[length(rows) + 1]] <- measure(areas$code[areas$is_core], "Urban Employment Area central city", areas)
  }
  district_year <- districts %>% filter(year == !!year)
  for (threshold in c(50000, 10000)) {
    codes <- district_year$muni_code[district_year$did_population >= threshold]
    rows[[length(rows) + 1]] <- measure(codes, sprintf("densely inhabited district population at least %s",
                                                       format(threshold, big.mark = ",", scientific = FALSE)))
  }
  bind_rows(rows)
}

results <- list()
for (year in census_years) {
  for (cutoff in cutoff_anchors) {
    message("core measures ", year, " at ", cutoff)
    results[[length(results) + 1]] <- core_measures(year, cutoff)
  }
}
core_table <- bind_rows(results)
write_csv(core_table, output_path("core_table.csv"))
print(as.data.frame(core_table %>% filter(cutoff == 0.980)))

# The map of urban areas split across commuting zones, the counterpart of Figure 6,
# drawn at each cutoff anchor.
headline_year <- 2020
for (headline_cutoff in cutoff_anchors) {
zones <- read_csv(zone_path(headline_year, headline_cutoff),
                  col_types = cols(code = col_character(), zone = col_integer()))
areas <- read_uea(headline_year) %>% filter(code %in% zones$code)
split_status <- areas %>%
  mutate(zone = unname(setNames(zones$zone, zones$code)[code])) %>%
  group_by(area) %>%
  mutate(is_split = n_distinct(zone) > 1) %>%
  ungroup()
if (abs(headline_cutoff - 0.977) < 1e-9) {
  write_csv(split_status, output_path("core_area_splits.csv"))
}

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"), col_types = cols(code = col_character())) %>%
  filter(in_scope)
geometry <- read_boundaries(scope$code)
okinawa_codes <- scope$code[scope$block == "Okinawa main island"]
placed <- inset_okinawa(geometry, okinawa_codes)
geometry <- placed$layer
okinawa_frame <- placed$frame

# Two dissolved layers laid over each other, as in the source paper: the urban areas
# that are split, and the commuting zones that split them. Unsplit areas are not drawn.
# The overlap is what carries the figure, so both layers are translucent and outlined in
# their own colour, and a split area is read as lying across a zone boundary.
split_areas <- geometry %>%
  inner_join(split_status %>% filter(is_split) %>% distinct(area, code), by = "code") %>%
  group_by(area) %>%
  summarise(.groups = "drop") %>%
  st_make_valid()

splitting_zones <- geometry %>%
  inner_join(zones, by = "code") %>%
  filter(zone %in% unique(split_status$zone[split_status$is_split])) %>%
  group_by(zone) %>%
  summarise(.groups = "drop") %>%
  st_make_valid()

land <- st_union(geometry)

area_colour <- "red"
zone_colour <- "blue"
ggplot() +
  geom_sf(data = land, fill = "white", colour = "grey60", linewidth = 0.15) +
  geom_sf(data = splitting_zones, aes(fill = "Commuting zones that split an urban area",
                                      colour = "Commuting zones that split an urban area"),
          alpha = 0.25, linewidth = 0.2) +
  geom_sf(data = split_areas, aes(fill = "Urban areas split across commuting zones",
                                  colour = "Urban areas split across commuting zones"),
          alpha = 0.25, linewidth = 0.2) +
  geom_sf(data = okinawa_frame, fill = NA, colour = "grey55", linewidth = 0.2) +
  scale_fill_manual(name = NULL, values = c(
    "Commuting zones that split an urban area" = zone_colour,
    "Urban areas split across commuting zones" = area_colour)) +
  scale_colour_manual(name = NULL, values = c(
    "Commuting zones that split an urban area" = zone_colour,
    "Urban areas split across commuting zones" = area_colour)) +
  theme_void(base_size = 19) +
  theme(legend.position = "inside", legend.position.inside = c(0.68, 0.14),
        legend.direction = "vertical", legend.background = element_blank(),
        legend.key.size = unit(0.9, "cm"),
        plot.margin = margin(2, 2, 2, 2),
        plot.background = element_rect(fill = "white", colour = NA))
ggsave(file.path(figure_dir, sprintf("split_urban_areas_%d_cut%s.png", headline_year,
                                     format(headline_cutoff, nsmall = 3))),
       width = 7.5, height = 7.5, dpi = 300, bg = "white")

message("cutoff ", format(headline_cutoff, nsmall = 3), ": urban areas in ", headline_year, ": ",
        n_distinct(split_status$area), "; split across zones: ",
        n_distinct(split_status$area[split_status$is_split]))
}
