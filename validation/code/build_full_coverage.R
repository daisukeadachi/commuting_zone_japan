# Commuting zones covering every municipality the census reports.
#
# The delineation the diagnostics run on covers the four main islands, the Okinawa main
# island and everything joined to one of them by a permanent road link. That leaves the
# offshore islands out, and with them 0.46 percent of the resident labour force in 2020.
# A user of the published zones wants the whole census population, so this builds the
# same delineation on the whole of it.
#
# Nothing about the method changes. The islands are further connected components of the
# adjacency graph, two municipalities on one island share a boundary, and the constrained
# clustering already runs on each component separately. Their zones therefore come out of
# the flows rather than out of an assumption that each island is a commuting zone, and
# their containment can be read off the result instead of being asserted.
#
# The resident labour force in the denominator of the proportional flow is the row sum
# over every destination the census records, rather than over the destinations inside the
# scope, so the dissimilarities on the main islands shift very slightly. How far that
# moves the delineation is reported rather than assumed.
#
# Writes validation/derived/zones_full_<year>_cut<cutoff>.csv, not committed, and
# validation/output/full_coverage_zones.csv and validation/output/island_zones.csv. All
# three carry the sample and the denominator in their names away from the baseline.

suppressMessages({
  library(dplyr)
  library(readr)
  library(igraph)
  library(adespatial)
})
source("validation/code/config.R")

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"),
                  col_types = cols(code = col_character()))
edges <- read_csv(file.path(data_dir, "adjacency_edges.csv"),
                  col_types = cols(.default = col_character()))
in_scope <- scope$code[scope$in_scope]
muni_name <- setNames(scope$muni_name, scope$code)

#' Proportional-flow dissimilarity on every municipality a census year reports.
#'
#' @param flows the year's commuting matrix, on the merged municipality universe
#' @param year census year, passed on to the denominator
#' @return a list with the dissimilarity matrix, the flow matrix and the labour force
full_dissimilarity <- function(flows, year) {
  # Municipalities that appear only as a destination have no resident labour force in the
  # matrix and cannot enter a delineation built on residents, so the universe is the set
  # of origins, as it is for the delineation the diagnostics run on.
  units <- sort(unique(flows$i))
  kept <- flows %>% filter(i %in% units, j %in% units)
  mat <- matrix(0, nrow = length(units), ncol = length(units), dimnames = list(units, units))
  mat[cbind(match(kept$i, units), match(kept$j, units))] <- kept$pop
  rlf <- resident_labour_force(flows, units, year)
  pair_min <- pmin(matrix(rlf, length(units), length(units), byrow = FALSE),
                   matrix(rlf, length(units), length(units), byrow = TRUE))
  prop <- (mat + t(mat)) / pair_min
  prop[prop >= 1 - dissimilarity_floor] <- 1 - dissimilarity_floor
  dissim <- 1 - prop
  diag(dissim) <- 0
  list(dissim = dissim, flows = mat, rlf = rlf, units = units)
}

#' Constrained tree for one connected component, or NULL when it holds one municipality.
component_tree <- function(dissim, units) {
  if (length(units) < 2) return(NULL)
  e <- edges %>% filter(code_i %in% units, code_j %in% units)
  tree <- constr.hclust(as.dist(dissim[units, units]), method = "average",
                        links = cbind(match(e$code_i, units), match(e$code_j, units)))
  stopifnot(!any(is.na(tree$height)))
  tree
}

#' Zone assignment for one year at one cutoff, over every component.
#'
#' Labels are offset component by component so that no two components share a zone
#' number, in the order the component identifiers sort, which fixes the numbering.
assign_zones <- function(trees, membership, cutoff) {
  offset <- 0
  bind_rows(lapply(names(trees), function(k) {
    members <- names(membership)[membership == as.integer(k)]
    zone <- if (is.null(trees[[k]])) setNames(1L, members) else cut_tree_at_height(trees[[k]], cutoff)
    out <- tibble(code = names(zone), zone = unname(zone) + offset)
    offset <<- offset + length(unique(zone))
    out
  })) %>% arrange(code)
}

summary_rows <- list()
island_rows <- list()

for (year in census_years) {
  message("full coverage ", year)
  flows <- read_commuting(year)
  built <- full_dissimilarity(flows, year)
  units <- built$units

  e <- edges %>% filter(code_i %in% units, code_j %in% units)
  graph <- graph_from_data_frame(e[, c("code_i", "code_j")], directed = FALSE,
                                 vertices = data.frame(name = units))
  membership <- components(graph)$membership
  trees <- lapply(sort(unique(membership)), function(k) {
    component_tree(built$dissim, names(membership)[membership == k])
  })
  names(trees) <- sort(unique(membership))

  # Containment is the share of a municipality's workers who work inside its own zone,
  # so it divides by the workers the flow matrix holds rather than by whatever count the
  # proportional flow divides by, which need not be the same people.
  workers <- rowSums(built$flows)
  contained_by <- function(assigned) {
    zone <- setNames(assigned$zone, assigned$code)
    same <- outer(zone[units], zone[units], `==`)
    rowSums(built$flows * same) / workers
  }

  for (cutoff in cutoff_anchors) {
    assigned <- assign_zones(trees, membership, cutoff)
    write_csv(assigned, full_zone_path(year, cutoff))

    contained <- contained_by(assigned)
    zone_of <- setNames(assigned$zone, assigned$code)
    island_units <- setdiff(units, in_scope)
    island_zone_ids <- sort(unique(zone_of[island_units]))

    # The delineation the diagnostics report, on the same municipalities, so that the
    # effect of widening the denominator of the proportional flow can be read off.
    baseline <- read_csv(zone_path(year, cutoff, "constrained"),
                         col_types = cols(code = col_character(), zone = col_integer()))
    shared <- intersect(baseline$code, assigned$code)
    agreement <- jaccard(setNames(assigned$zone, assigned$code)[shared],
                         setNames(baseline$zone, baseline$code)[shared])

    for (z in island_zone_ids) {
      members <- names(zone_of)[zone_of == z]
      island_rows[[length(island_rows) + 1]] <- tibble(
        year = year, cutoff = cutoff, zone = z,
        municipalities = length(members),
        muni_names = paste(muni_name[members], collapse = "; "),
        resident_labour_force = sum(workers[members]),
        mean_contained = mean(contained[members]),
        labour_weighted_contained = weighted.mean(contained[members], workers[members]))
    }

    summary_rows[[length(summary_rows) + 1]] <- tibble(
      year = year, cutoff = cutoff,
      municipalities = length(units),
      municipalities_on_islands = length(island_units),
      zones = n_distinct(assigned$zone),
      island_zones = length(island_zone_ids),
      labour_force = sum(workers),
      labour_force_on_islands = sum(workers[island_units]),
      share_of_labour_force_on_islands = sum(workers[island_units]) / sum(workers),
      mean_contained_islands = mean(contained[island_units]),
      mean_contained_in_scope = mean(contained[intersect(units, in_scope)]),
      mean_similarity_to_baseline = mean(agreement),
      municipalities_in_a_changed_zone = sum(agreement < 1))
  }
}

summary_table <- bind_rows(summary_rows)
write_csv(summary_table, output_path("full_coverage_zones.csv"))
write_csv(bind_rows(island_rows), output_path("island_zones.csv"))
print(as.data.frame(summary_table))
