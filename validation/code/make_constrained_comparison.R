# The constrained delineation set beside the unconstrained one at the same cutoff.
#
# Both trees are built from the same proportional-flow dissimilarity, and their merge
# heights are average dissimilarities computed from it, so a cut at a common height means
# the same thing in both: a zone is a group whose average proportional flow across its
# last merge exceeded one minus the cutoff. The comparison is therefore read at equal
# cutoffs rather than at equal zone counts, and the counts themselves are a result.
#
# Reported for every census year and both cutoff anchors: the zone counts on each side,
# the municipality-level Jaccard similarity between the two partitions, the labour force
# sitting in a zone whose membership changes, containment on each side, and the count of
# non-contiguous zones on each side. The last is zero on the constrained side by
# construction and serves as a check on the implementation.
#
# Containment is what measures the cost of the constraint. A zone that the constraint
# forces apart, or forces together, fits the flows worse, and containment is the share of
# a municipality's residents who work inside their own zone.
#
# Writes validation/output/constrained_comparison.csv and
# validation/output/constrained_movers.csv.

suppressMessages({
  library(dplyr)
  library(readr)
  library(igraph)
})
source("validation/code/config.R")

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"),
                  col_types = cols(code = col_character())) %>% filter(in_scope)
edges <- read_csv(file.path(data_dir, "adjacency_edges.csv"),
                  col_types = cols(.default = col_character()))
neighbours <- split(c(edges$code_j, edges$code_i), c(edges$code_i, edges$code_j))

#' Non-contiguity of a partition, on the two tests the diagnostics table carries.
#'
#' The first is the source paper's test, shared with the diagnostics table. The second is
#' stricter: a zone is broken when its municipalities do not form a connected subgraph of
#' the adjacency graph.
noncontiguity <- function(zone) {
  detached <- detached_municipalities(zone, neighbours)
  year_edges <- edges %>% filter(code_i %in% names(zone), code_j %in% names(zone))
  g <- graph_from_data_frame(year_edges[, c("code_i", "code_j")], directed = FALSE,
                             vertices = data.frame(name = names(zone)))
  broken <- vapply(split(names(zone), zone), function(members) {
    if (length(members) == 1) return(FALSE)
    components(induced_subgraph(g, members))$no > 1
  }, logical(1))
  list(paper_test = length(unique(zone[detached])), broken = sum(broken))
}

#' Containment of a partition, on the definitions of the containment table.
containment <- function(zone, flows) {
  f <- flows %>% mutate(same = zone[i] == zone[j])
  by_residence <- f %>%
    group_by(code = i) %>%
    summarise(residents = sum(pop), residents_in_zone = sum(pop[same]), .groups = "drop") %>%
    mutate(contained = residents_in_zone / residents, zone = zone[code])
  by_zone <- by_residence %>%
    group_by(zone) %>%
    summarise(residents = sum(residents), mean_contained = mean(contained), .groups = "drop")
  list(
    mean_contained = mean(by_zone$mean_contained),
    labour_weighted_mean_contained = weighted.mean(by_zone$mean_contained, by_zone$residents),
    share_of_labour_force_contained = sum(by_residence$residents_in_zone) / sum(by_residence$residents)
  )
}

movers <- list()

rows <- bind_rows(lapply(census_years, function(year) {
  message("comparing ", year)
  labour <- read_csv(file.path(derived_dir, sprintf("labour_force_%d.csv", year)),
                     col_types = cols(code = col_character()))
  tree <- readRDS(file.path(derived_dir, sprintf("hclust_%d.rds", year)))
  all_flows <- read_commuting(year)

  bind_rows(lapply(cutoff_anchors, function(cutoff) {
    constrained <- read_csv(zone_path(year, cutoff, "constrained"),
                            col_types = cols(code = col_character(), zone = col_integer()))
    units <- constrained$code
    free <- cutree(tree, h = cutoff)[units]
    fixed <- setNames(constrained$zone, units)
    weight <- labour$rlf_in_scope[match(units, labour$code)]
    flows <- all_flows %>% filter(i %in% units, j %in% units)

    score <- jaccard(free, fixed)
    moved <- score < 1
    nc_free <- noncontiguity(free)
    nc_fixed <- noncontiguity(fixed)
    ct_free <- containment(free, flows)
    ct_fixed <- containment(fixed, flows)

    if (any(moved)) {
      # For a municipality whose zone membership changes, the share of its commuters
      # reaching each of the two zones says which delineation fits its flows better.
      outgoing <- flows %>% filter(i %in% units[moved])
      share_to <- function(assignment) {
        vapply(units[moved], function(code) {
          own <- outgoing %>% filter(i == code)
          sum(own$pop[assignment[own$j] == assignment[code]]) / sum(own$pop)
        }, numeric(1))
      }
      movers[[length(movers) + 1]] <<- tibble(
        year = year, cutoff = cutoff, code = units[moved],
        muni_name = scope$muni_name[match(units[moved], scope$code)],
        prefecture = scope$prefecture[match(units[moved], scope$code)],
        rlf_in_scope = weight[moved], similarity = unname(score[moved]),
        unconstrained_zone_size = as.integer(table(free)[as.character(free[moved])]),
        constrained_zone_size = as.integer(table(fixed)[as.character(fixed[moved])]),
        share_commuting_to_unconstrained_zone = unname(share_to(free)),
        share_commuting_to_constrained_zone = unname(share_to(fixed)))
    }

    tibble(
      year = year, cutoff = cutoff, municipalities = length(units),
      zones_unconstrained = n_distinct(free), zones_constrained = n_distinct(fixed),
      mean_similarity = mean(score),
      labour_weighted_similarity = weighted.mean(score, weight),
      municipalities_in_a_changed_zone = sum(moved),
      share_of_labour_force_in_a_changed_zone = sum(weight[moved]) / sum(weight),
      mean_contained_unconstrained = ct_free$mean_contained,
      mean_contained_constrained = ct_fixed$mean_contained,
      labour_weighted_contained_unconstrained = ct_free$labour_weighted_mean_contained,
      labour_weighted_contained_constrained = ct_fixed$labour_weighted_mean_contained,
      share_contained_unconstrained = ct_free$share_of_labour_force_contained,
      share_contained_constrained = ct_fixed$share_of_labour_force_contained,
      noncontiguous_zones_unconstrained = nc_free$paper_test,
      noncontiguous_zones_constrained = nc_fixed$paper_test,
      broken_zones_unconstrained = nc_free$broken,
      broken_zones_constrained = nc_fixed$broken)
  }))
}))

write_csv(rows, file.path(output_dir, "constrained_comparison.csv"))
write_csv(bind_rows(movers), file.path(output_dir, "constrained_movers.csv"))
print(as.data.frame(rows))
