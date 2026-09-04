# Contiguity-constrained hierarchical clustering of the proportional-flow dissimilarity,
# following Guenard and Legendre (2022), <https://doi.org/10.18637/jss.v103.i07>.
#
# The algorithm is average linkage restricted to pairs of clusters that are adjacent on
# the municipality graph, with the adjacency of a merged cluster taken as the union of
# the two it came from. The merge heights are ordinary average dissimilarities computed
# from the same matrix the unconstrained tree uses, so a cut at a given height carries
# the same meaning in both and the two delineations are read at the same cutoff.
#
# The clustering runs separately on each connected component of the graph, which the
# implementation forces rather than merely permits: given a disconnected graph
# constr.hclust cannot execute the last merges and returns them with a height of NA,
# and cutree then fails on a height. The components are recomputed from each year's own
# municipality set rather than read from the scope table, because a municipality absent
# in one census year could in principle disconnect the graph for that year alone.
#
# Writes validation/derived/constrained_hclust_<year>.rds and
# validation/derived/zones_constrained_<year>_cut<cutoff>.csv, neither committed, and
# validation/output/constrained_inversions.csv. All three carry the labour-force sample
# in their names away from the baseline.

suppressMessages({
  library(dplyr)
  library(readr)
  library(igraph)
  library(adespatial)
})
source("validation/code/config.R")

# The adjacency graph is read per year because under each census date's own municipality
# codes it is a different graph. Under the harmonized codes it is the same edge list every
# time.

#' Connected components of the adjacency graph induced on one year's municipalities.
#'
#' @param units character vector of municipality codes present in that year
#' @param edges the year's adjacency edge list
#' @return named integer vector, one component label per municipality
year_components <- function(units, edges) {
  e <- edges %>% filter(code_i %in% units, code_j %in% units)
  g <- graph_from_data_frame(e[, c("code_i", "code_j")], directed = FALSE,
                             vertices = data.frame(name = units))
  components(g)$membership
}

#' Constrained tree for one component.
#'
#' @param dissim_matrix full dissimilarity matrix for the year
#' @param units municipality codes of this component
#' @param edges the year's adjacency edge list
#' @return an object of class constr.hclust, or NULL when the component holds one unit
component_tree <- function(dissim_matrix, units, edges) {
  if (length(units) < 2) return(NULL)
  e <- edges %>% filter(code_i %in% units, code_j %in% units)
  tree <- constr.hclust(as.dist(dissim_matrix[units, units]), method = "average",
                        links = cbind(match(e$code_i, units), match(e$code_j, units)))
  stopifnot(!any(is.na(tree$height)))
  tree
}

inversion_rows <- list()

build_year <- function(year) {
  edges <- read_adjacency(year)
  dissim <- readRDS(derived_path("dissimilarity", year))
  units <- labels(dissim)
  dissim_matrix <- as.matrix(dissim)
  membership <- year_components(units, edges)
  component_ids <- sort(unique(membership))

  trees <- lapply(component_ids, function(k) {
    component_tree(dissim_matrix, names(membership)[membership == k], edges)
  })
  names(trees) <- component_ids

  for (k in names(trees)) {
    tree <- trees[[k]]
    if (is.null(tree)) next
    for (s in which(diff(tree$height) < -1e-12)) {
      inversion_rows[[length(inversion_rows) + 1]] <<- tibble(
        year = year, component = as.integer(k), municipalities = length(tree$labels),
        step = s, height_before = tree$height[s], height_after = tree$height[s + 1],
        drop = tree$height[s] - tree$height[s + 1],
        straddles_an_anchor = any(tree$height[s] > cutoff_anchors &
                                    tree$height[s + 1] <= cutoff_anchors))
    }
  }

  saveRDS(list(trees = trees, membership = membership),
          tree_path(year, "constrained"))

  bind_rows(lapply(cutoff_anchors, function(cutoff) {
    # Each component is cut on its own tree; the labels are then offset so that no two
    # components share a zone number. A one-municipality component is its own zone.
    offset <- 0
    assignments <- lapply(names(trees), function(k) {
      members <- names(membership)[membership == as.integer(k)]
      zone <- if (is.null(trees[[k]])) setNames(1L, members) else cut_tree_at_height(trees[[k]], cutoff)
      out <- tibble(code = names(zone), zone = unname(zone) + offset)
      offset <<- offset + length(unique(zone))
      out
    })
    assigned <- bind_rows(assignments) %>% arrange(code)
    write_csv(assigned, zone_path(year, cutoff, "constrained"))
    sizes <- table(assigned$zone)
    tibble(year = year, cutoff = cutoff, components = length(trees),
           municipalities = nrow(assigned), zones = length(sizes),
           singleton_zones = sum(sizes == 1), largest_zone = max(sizes))
  }))
}

counts <- bind_rows(lapply(census_years, function(y) {
  message("constrained clustering ", y)
  build_year(y)
}))
write_csv(counts, file.path(derived_dir,
                            paste0("constrained_cluster_counts", variant_tag(), ".csv")))
write_csv(bind_rows(inversion_rows), output_path("constrained_inversions.csv"))
print(as.data.frame(counts))
