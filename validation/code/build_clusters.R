# Hierarchical clustering of the proportional-flow dissimilarity, one tree per census
# year, and the commuting zone assignments at the two cutoff anchors.
#
# Average linkage and a cut at a fixed tree height follow Tolbert and Sizer (1996) and
# the source paper. The tree itself is kept because the cutoff sweep needs it.
#
# Writes validation/derived/hclust_<year>.rds and
# validation/derived/zones_<year>_cut<cutoff>.csv, both carrying the labour-force sample
# in their names away from the baseline. Neither is committed.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

counts <- lapply(census_years, function(year) {
  dissim <- readRDS(derived_path("dissimilarity", year))
  tree <- hclust(dissim, method = "average")
  saveRDS(tree, tree_path(year, "unconstrained"))

  bind_rows(lapply(cutoff_anchors, function(cutoff) {
    zone <- cutree(tree, h = cutoff)
    write_csv(tibble(code = names(zone), zone = unname(zone)),
              zone_path(year, cutoff, "unconstrained"))
    tibble(year = year, cutoff = cutoff, municipalities = length(zone),
           zones = length(unique(zone)),
           singleton_zones = sum(table(zone) == 1),
           largest_zone = max(table(zone)))
  }))
})

counts <- bind_rows(counts)
write_csv(counts, file.path(derived_dir, paste0("cluster_counts", variant_tag(), ".csv")))
print(as.data.frame(counts))
