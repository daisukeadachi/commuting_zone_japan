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
# validation/output/constrained_inversions.csv.

suppressMessages({
  library(dplyr)
  library(readr)
  library(igraph)
  library(adespatial)
})
source("validation/code/config.R")

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"),
                  col_types = cols(code = col_character())) %>% filter(in_scope)
edges <- read_csv(file.path(data_dir, "adjacency_edges.csv"),
                  col_types = cols(.default = col_character()))

#' Connected components of the adjacency graph induced on one year's municipalities.
#'
#' @param units character vector of municipality codes present in that year
#' @return named integer vector, one component label per municipality
year_components <- function(units) {
  e <- edges %>% filter(code_i %in% units, code_j %in% units)
  g <- graph_from_data_frame(e[, c("code_i", "code_j")], directed = FALSE,
                             vertices = data.frame(name = units))
  components(g)$membership
}

#' Constrained tree for one component.
#'
#' @param dissim_matrix full dissimilarity matrix for the year
#' @param units municipality codes of this component
#' @return an object of class constr.hclust, or NULL when the component holds one unit
component_tree <- function(dissim_matrix, units) {
  if (length(units) < 2) return(NULL)
  e <- edges %>% filter(code_i %in% units, code_j %in% units)
  tree <- constr.hclust(as.dist(dissim_matrix[units, units]), method = "average",
                        links = cbind(match(e$code_i, units), match(e$code_j, units)))
  stopifnot(!any(is.na(tree$height)))
  tree
}

#' Zones obtained by stopping the agglomeration at a cutoff.
#'
#' The merges are accepted in the order the algorithm made them, up to the first one
#' whose height exceeds the cutoff. Under the constraint the heights need not increase:
#' when a cluster is adjacent to one half of a merging pair and not to the other, only
#' the adjacent half is known to lie above the merge height, and the average of the two
#' can fall below it. A later merge cheaper than the cutoff is nevertheless refused,
#' because it became available only through the merge that stopped the agglomeration.
#' Where the heights do increase this is exactly cutree at that height.
#'
#' @param tree an object returned by constr.hclust
#' @param cutoff tree height at which to stop
#' @return named integer vector, one zone label per municipality
cut_at_height <- function(tree, cutoff) {
  n <- length(tree$labels)
  parent <- seq_len(n)
  root <- function(i) { while (parent[i] != i) i <- parent[i]; i }
  above <- which(tree$height > cutoff)
  accepted <- if (length(above)) seq_len(above[1] - 1L) else seq_len(nrow(tree$merge))
  representative <- integer(nrow(tree$merge))
  for (s in accepted) {
    side <- vapply(1:2, function(j) {
      if (tree$merge[s, j] < 0) -tree$merge[s, j] else representative[tree$merge[s, j]]
    }, integer(1))
    a <- root(side[1]); b <- root(side[2])
    parent[b] <- a
    representative[s] <- a
  }
  roots <- vapply(seq_len(n), root, integer(1))
  setNames(match(roots, unique(roots)), tree$labels)
}

inversion_rows <- list()

build_year <- function(year) {
  dissim <- readRDS(file.path(derived_dir, sprintf("dissimilarity_%d.rds", year)))
  units <- labels(dissim)
  dissim_matrix <- as.matrix(dissim)
  membership <- year_components(units)
  component_ids <- sort(unique(membership))

  trees <- lapply(component_ids, function(k) {
    component_tree(dissim_matrix, names(membership)[membership == k])
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
          file.path(derived_dir, sprintf("constrained_hclust_%d.rds", year)))

  bind_rows(lapply(cutoff_anchors, function(cutoff) {
    # Each component is cut on its own tree; the labels are then offset so that no two
    # components share a zone number. A one-municipality component is its own zone.
    offset <- 0
    assignments <- lapply(names(trees), function(k) {
      members <- names(membership)[membership == as.integer(k)]
      zone <- if (is.null(trees[[k]])) setNames(1L, members) else cut_at_height(trees[[k]], cutoff)
      out <- tibble(code = names(zone), zone = unname(zone) + offset)
      offset <<- offset + length(unique(zone))
      out
    })
    assigned <- bind_rows(assignments) %>% arrange(code)
    write_csv(assigned, file.path(derived_dir,
                                  sprintf("zones_constrained_%d_cut%s.csv", year, format(cutoff, nsmall = 3))))
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
write_csv(counts, file.path(derived_dir, "constrained_cluster_counts.csv"))
write_csv(bind_rows(inversion_rows), file.path(output_dir, "constrained_inversions.csv"))
print(as.data.frame(counts))
