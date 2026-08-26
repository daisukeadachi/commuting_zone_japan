# Similarity between delineations of two census years, and the cutoff sweep.
#
# Similarity for municipality c is the Jaccard index between the set of municipalities
# sharing c's commuting zone in the earlier year and the set sharing it in the later
# year, each set including c itself. It is reported unweighted across municipalities,
# weighted by resident labour force, and as the labour-force weighted mean.
#
# The sweep is asymmetric, following the source paper: the earlier year is held at an
# anchor cutoff and only the later year's cutoff varies. A common cutoff on both years
# would make the objective degenerate, since similarity tends to one at both ends of the
# range. Alongside the similarity the sweep records the diagnostic profile, so that what
# a move in the cutoff trades away is visible.
#
# Writes validation/output/similarity_table.csv,
# validation/output/similarity_by_municipality.csv and
# validation/output/cutoff_sweep.csv.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"), col_types = cols(code = col_character())) %>%
  filter(in_scope)
edges <- read_csv(file.path(data_dir, "adjacency_edges.csv"), col_types = cols(.default = col_character()))
neighbours <- split(c(edges$code_j, edges$code_i), c(edges$code_i, edges$code_j))

trees <- setNames(lapply(census_years, function(y) readRDS(file.path(derived_dir, sprintf("hclust_%d.rds", y)))),
                  as.character(census_years))
labour <- setNames(lapply(census_years, function(y) {
  read_csv(file.path(derived_dir, sprintf("labour_force_%d.csv", y)), col_types = cols(code = col_character()))
}), as.character(census_years))

# Jaccard similarity for every municipality, from the cross-tabulation of the two
# partitions: the intersection of the two co-membership sets is the count of
# municipalities sharing both zones, and the union follows from the two zone sizes.
jaccard <- function(zone_a, zone_b) {
  counts <- table(zone_a, zone_b)
  size_a <- rowSums(counts)
  size_b <- colSums(counts)
  both <- counts[cbind(match(zone_a, rownames(counts)), match(zone_b, colnames(counts)))]
  both / (size_a[match(zone_a, names(size_a))] + size_b[match(zone_b, names(size_b))] - both)
}

common_units <- function(earlier, later) {
  intersect(trees[[as.character(earlier)]]$labels, trees[[as.character(later)]]$labels)
}

similarity <- function(earlier, later, cut_earlier, cut_later) {
  units <- common_units(earlier, later)
  a <- cutree(trees[[as.character(earlier)]], h = cut_earlier)[units]
  b <- cutree(trees[[as.character(later)]], h = cut_later)[units]
  score <- jaccard(a, b)
  weight_earlier <- labour[[as.character(earlier)]]$rlf_in_scope[match(units, labour[[as.character(earlier)]]$code)]
  weight_later <- labour[[as.character(later)]]$rlf_in_scope[match(units, labour[[as.character(later)]]$code)]
  list(
    units = units, score = unname(score), zone_earlier = a, zone_later = b,
    summary = tibble(
      earlier_year = earlier, later_year = later,
      cutoff_earlier = cut_earlier, cutoff_later = cut_later,
      municipalities = length(units),
      mean_similarity = mean(score),
      labour_weighted_similarity_earlier = weighted.mean(score, weight_earlier),
      labour_weighted_similarity_later = weighted.mean(score, weight_later),
      sd_similarity = sd(score)
    )
  )
}

pairs <- unique(c(baseline_pairs, appendix_pairs))
pair_rows <- list()
municipality_rows <- list()
for (pair in pairs) {
  for (anchor in cutoff_anchors) {
    result <- similarity(pair[1], pair[2], anchor, anchor)
    pair_rows[[length(pair_rows) + 1]] <- result$summary %>%
      mutate(role = if (list(pair) %in% baseline_pairs) "baseline" else "appendix")
    municipality_rows[[length(municipality_rows) + 1]] <- tibble(
      earlier_year = pair[1], later_year = pair[2], cutoff = anchor,
      code = result$units, similarity = result$score,
      zone_earlier = unname(result$zone_earlier), zone_later = unname(result$zone_later)
    )
  }
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(bind_rows(pair_rows), file.path(output_dir, "similarity_table.csv"))
write_csv(bind_rows(municipality_rows), file.path(output_dir, "similarity_by_municipality.csv"))
print(as.data.frame(bind_rows(pair_rows)))

# The sweep. The earlier year sits at the anchor and the later year's cutoff runs over
# the range the source paper uses.
sweep_grid <- seq(0.925, 0.999, by = 0.001)
sweep_rows <- list()
for (pair in baseline_pairs) {
  later <- pair[2]
  flows <- read_commuting(later) %>%
    filter(i %in% trees[[as.character(later)]]$labels, j %in% trees[[as.character(later)]]$labels)
  for (anchor in cutoff_anchors) {
    message("sweeping ", pair[1], " against ", later, " anchored at ", anchor)
    for (cut_later in sweep_grid) {
      result <- similarity(pair[1], later, anchor, cut_later)
      zone_all <- cutree(trees[[as.character(later)]], h = cut_later)
      sizes <- table(zone_all)
      zone_of <- zone_all
      detached <- vapply(names(zone_of), function(m) {
        if (sizes[as.character(zone_of[m])] == 1) return(FALSE)
        !any(zone_of[neighbours[[m]]] == zone_of[m], na.rm = TRUE)
      }, logical(1))
      contained <- flows %>%
        mutate(same = zone_of[i] == zone_of[j]) %>%
        summarise(share = sum(pop[same]) / sum(pop)) %>%
        pull(share)
      sweep_rows[[length(sweep_rows) + 1]] <- tibble(
        earlier_year = pair[1], later_year = later, anchor = anchor, cutoff_later = cut_later,
        mean_similarity = result$summary$mean_similarity,
        sd_similarity = result$summary$sd_similarity,
        labour_weighted_similarity = result$summary$labour_weighted_similarity_later,
        commuting_zones = length(sizes),
        single_municipality_zones = sum(sizes == 1),
        largest_zone = max(sizes),
        noncontiguous_zones = n_distinct(zone_of[detached]),
        share_of_labour_force_contained = contained
      )
    }
  }
}
write_csv(bind_rows(sweep_rows), file.path(output_dir, "cutoff_sweep.csv"))
message("sweep rows: ", length(sweep_rows))
