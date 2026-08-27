# Reassignments that would remove non-contiguity, the counterpart of Table 3 of the
# source paper. These are a diagnostic only. The delineation itself keeps every
# municipality where the clustering put it, which is the source paper's own stance.
#
# For each municipality that shares its commuting zone with none of its neighbours, the
# table gives the zone it sits in, the neighbouring zone that receives the largest share
# of its outgoing commuters, the share going there against the share going to its own
# zone, and the straight-line distance to the nearest part of its own zone.
#
# Writes validation/output/reassignment_table.csv.

suppressMessages({
  library(sf)
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")
sf_use_s2(FALSE)

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"), col_types = cols(code = col_character())) %>%
  filter(in_scope)
edges <- read_csv(file.path(data_dir, "adjacency_edges.csv"), col_types = cols(.default = col_character()))
neighbours <- split(c(edges$code_j, edges$code_i), c(edges$code_i, edges$code_j))

geometry <- read_boundaries(scope$code)

detached <- read_csv(output_path("noncontiguous_municipalities.csv"),
                     col_types = cols(code = col_character()))

describe <- function(year, cutoff, cases) {
  zones <- read_csv(zone_path(year, cutoff),
                    col_types = cols(code = col_character(), zone = col_integer()))
  zone_of <- setNames(zones$zone, zones$code)
  flows <- read_commuting(year) %>% filter(i %in% zones$code, j %in% zones$code)

  bind_rows(lapply(seq_len(nrow(cases)), function(k) {
    unit <- cases$code[k]
    own_zone <- zone_of[[unit]]
    neighbour_zones <- unique(zone_of[neighbours[[unit]]])
    outgoing <- flows %>% filter(i == unit)
    total <- sum(outgoing$pop)
    by_zone <- outgoing %>%
      mutate(zone = zone_of[j]) %>%
      group_by(zone) %>%
      summarise(commuters = sum(pop), .groups = "drop")
    adjacent <- by_zone %>%
      filter(zone %in% neighbour_zones) %>%
      arrange(zone) %>%
      slice_max(commuters, n = 1, with_ties = FALSE)
    own <- by_zone %>% filter(zone == own_zone)

    unit_shape <- geometry %>% filter(code == unit)
    rest_of_zone <- geometry %>% filter(code %in% setdiff(zones$code[zones$zone == own_zone], unit))
    gap_km <- if (nrow(rest_of_zone) == 0) NA_real_ else
      as.numeric(min(st_distance(unit_shape, rest_of_zone))) / 1000

    tibble(
      year = year, cutoff = cutoff, code = unit,
      muni_name = scope$muni_name[match(unit, scope$code)],
      prefecture = scope$prefecture[match(unit, scope$code)],
      assigned_zone = own_zone,
      share_commuting_to_assigned_zone = if (nrow(own)) own$commuters / total else 0,
      nearest_adjacent_zone = if (nrow(adjacent)) adjacent$zone else NA_integer_,
      share_commuting_to_that_zone = if (nrow(adjacent)) adjacent$commuters / total else NA_real_,
      distance_to_assigned_zone_km = gap_km,
      suggestion = if (!nrow(adjacent)) "no adjacent zone to move to" else
        if (adjacent$commuters > (if (nrow(own)) own$commuters else 0))
          "move to the adjacent zone it commutes to most" else
          "keep the assigned zone, which receives more of its commuters"
    )
  }))
}

cases <- detached %>% distinct(year, cutoff)
rows <- lapply(seq_len(nrow(cases)), function(k) {
  here <- detached %>% filter(year == cases$year[k], cutoff == cases$cutoff[k])
  describe(cases$year[k], cases$cutoff[k], here)
})

# A contiguity-constrained delineation leaves nothing to report, and an empty list of
# rows would write a file carrying no header at all. The columns are written whether or
# not any municipality is detached, so that the file always states what it would hold.
empty <- tibble(
  year = integer(), cutoff = double(), code = character(), muni_name = character(),
  prefecture = character(), assigned_zone = integer(),
  share_commuting_to_assigned_zone = double(), nearest_adjacent_zone = integer(),
  share_commuting_to_that_zone = double(), distance_to_assigned_zone_km = double(),
  suggestion = character())

table <- if (length(rows)) bind_rows(rows) else empty
write_csv(table, output_path("reassignment_table.csv"))
print(as.data.frame(table))
