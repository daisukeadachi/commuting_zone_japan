# Hierarchical clustering of the proportional-flow dissimilarity, one tree per census
# year, and the commuting zone assignments at the two cutoff anchors.
#
# Average linkage and a cut at a fixed tree height follow Tolbert and Sizer (1996) and
# the source paper. The tree itself is kept because the cutoff sweep needs it.
#
# Writes validation/derived/hclust_<year>.rds and
# validation/derived/zones_<year>_cut<cutoff>.csv. Neither is committed.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

counts <- lapply(census_years, function(year) {
  dissim <- readRDS(file.path(derived_dir, sprintf("dissimilarity_%d.rds", year)))
  tree <- hclust(dissim, method = "average")
  saveRDS(tree, file.path(derived_dir, sprintf("hclust_%d.rds", year)))

  bind_rows(lapply(cutoff_anchors, function(cutoff) {
    zone <- cutree(tree, h = cutoff)
    write_csv(tibble(code = names(zone), zone = unname(zone)),
              file.path(derived_dir, sprintf("zones_%d_cut%s.csv", year, format(cutoff, nsmall = 3))))
    tibble(year = year, cutoff = cutoff, municipalities = length(zone),
           zones = length(unique(zone)),
           singleton_zones = sum(table(zone) == 1),
           largest_zone = max(table(zone)))
  }))
})

counts <- bind_rows(counts)
write_csv(counts, file.path(derived_dir, "cluster_counts.csv"))
print(as.data.frame(counts))
