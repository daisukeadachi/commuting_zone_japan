# Diagnostic statistics for each delineation: the counterpart of Table 1 of the source
# paper, reported for every census year at both cutoff anchors.
#
# Population is the resident labour force carried in the commuting matrix, since actual
# census population by municipality is available for one year only. Area and compactness
# are computed on the equal-area projection declared in config.R. Compactness is the
# Polsby-Popper ratio of the dissolved commuting zone polygon, averaged across zones.
#
# Writes validation/output/diagnostics_table.csv and, for the contiguity diagnostics of
# issue #11, validation/output/noncontiguous_municipalities.csv.

suppressMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(igraph)
})
source("validation/code/config.R")
sf_use_s2(FALSE)

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"), col_types = cols(code = col_character())) %>%
  filter(in_scope)
edges <- read_csv(file.path(data_dir, "adjacency_edges.csv"), col_types = cols(.default = col_character()))

geometry <- read_boundaries(scope$code)

neighbours <- split(c(edges$code_j, edges$code_i), c(edges$code_i, edges$code_j))

diagnose <- function(year, cutoff) {
  zones <- read_csv(zone_path(year, cutoff),
                    col_types = cols(code = col_character(), zone = col_integer()))
  labour <- read_csv(derived_path("labour_force", year, ".csv"),
                     col_types = cols(code = col_character()))
  units <- zones %>%
    left_join(labour[, c("code", "rlf_in_scope")], by = "code") %>%
    left_join(scope[, c("code", "area_km2", "muni_name")], by = "code")
  zone_size <- table(units$zone)

  # A municipality is non-contiguous when no neighbour of its own shares its commuting
  # zone, following the source paper's test. A municipality alone in its zone counts as
  # contiguous by construction.
  zone_of <- setNames(units$zone, units$code)
  units$detached <- unname(detached_municipalities(zone_of, neighbours))

  # The stricter reading: a zone is non-contiguous when its municipalities do not form a
  # connected subgraph of the adjacency graph.
  year_edges <- edges %>% filter(code_i %in% units$code, code_j %in% units$code)
  zone_graph <- graph_from_data_frame(year_edges[, c("code_i", "code_j")], directed = FALSE,
                                      vertices = tibble(name = units$code))
  broken <- vapply(split(units$code, units$zone), function(members) {
    if (length(members) == 1) return(FALSE)
    components(induced_subgraph(zone_graph, members))$no > 1
  }, logical(1))

  zone_area <- units %>% group_by(zone) %>% summarise(area = sum(area_km2), pop = sum(rlf_in_scope), .groups = "drop")

  dissolved <- geometry %>%
    inner_join(units[, c("code", "zone")], by = "code") %>%
    group_by(zone) %>%
    summarise(.groups = "drop") %>%
    st_make_valid()
  perimeter <- as.numeric(st_length(st_cast(st_geometry(dissolved), "MULTILINESTRING")))
  polsby_popper <- 4 * pi * as.numeric(st_area(dissolved)) / perimeter^2

  list(
    row = tibble(
      year = year,
      cutoff = cutoff,
      municipalities = nrow(units),
      commuting_zones = length(zone_size),
      single_municipality_zones = sum(zone_size == 1),
      municipalities_in_largest_zone = max(zone_size),
      noncontiguous_zones_paper_test = n_distinct(units$zone[units$detached]),
      noncontiguous_zones_connected_subgraph = sum(broken),
      detached_municipalities = sum(units$detached),
      detached_share_of_labour_force = sum(units$rlf_in_scope[units$detached]) / sum(units$rlf_in_scope),
      min_population = min(zone_area$pop),
      mean_population = mean(zone_area$pop),
      max_population = max(zone_area$pop),
      min_area_km2 = min(zone_area$area),
      mean_area_km2 = mean(zone_area$area),
      max_area_km2 = max(zone_area$area),
      compactness = mean(polsby_popper)
    ),
    detached = units %>%
      filter(detached) %>%
      transmute(year, cutoff, code, muni_name, zone, rlf_in_scope)
  )
}

results <- list()
for (year in census_years) {
  for (cutoff in cutoff_anchors) {
    message("diagnosing ", year, " at ", cutoff)
    results[[length(results) + 1]] <- diagnose(year, cutoff)
  }
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(bind_rows(lapply(results, `[[`, "row")), output_path("diagnostics_table.csv"))
write_csv(bind_rows(lapply(results, `[[`, "detached")), output_path("noncontiguous_municipalities.csv"))
print(as.data.frame(bind_rows(lapply(results, `[[`, "row"))[, 1:10]))
