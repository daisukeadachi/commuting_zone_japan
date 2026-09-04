# Proportional-flow dissimilarity matrices, one per census year.
#
# For municipalities i and j the proportional flow is the sum of the two directed
# commuting flows divided by the smaller of the two resident labour forces, capped at
# 0.999, and the dissimilarity is one minus that. The cap is what the existing Japanese
# pipeline writes as a floor of 0.001 on the dissimilarity; the two are algebraically
# the same.
#
# The resident labour force is the row sum of the commuting matrix over every destination
# the census records, which is how the source paper computes the total workforce it
# divides by. Which count is used is the denominator setting in config.R, and the flows
# in the numerator are unaffected by it.
#
# Writes validation/derived/dissimilarity_<year>.rds and
# validation/derived/labour_force_<year>.csv, both carrying the labour-force sample in
# their names away from the baseline. Neither is committed.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

dir.create(derived_dir, showWarnings = FALSE, recursive = TRUE)

build_year <- function(year) {
  # The scope is read per year because under each census date's own municipality codes it
  # is a different set of units. Under the harmonized codes it is the same table every
  # time.
  scope <- read_scope(year)
  in_scope <- scope$code[scope$in_scope]
  flows <- read_commuting(year)

  reported <- flows %>% group_by(i) %>% summarise(rlf_reported = sum(pop), .groups = "drop")
  units <- sort(intersect(unique(flows$i), in_scope))
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

  saveRDS(as.dist(dissim), derived_path("dissimilarity", year))

  labour <- tibble(code = units, denominator = rlf, rlf_in_scope = rowSums(mat)) %>%
    left_join(reported, by = c("code" = "i")) %>%
    mutate(dropped_to_out_of_scope = rlf_reported - rlf_in_scope)
  write_csv(labour, derived_path("labour_force", year, ".csv"))

  tibble(
    year = year,
    municipalities_reported = length(unique(flows$i)),
    municipalities_in_scope = length(units),
    labour_force_in_denominator = sum(rlf),
    labour_force_in_scope = sum(labour$rlf_in_scope),
    labour_force_reported = sum(labour$rlf_reported),
    share_lost_to_out_of_scope_destinations =
      1 - sum(labour$rlf_in_scope) / sum(labour$rlf_reported)
  )
}

summary_table <- bind_rows(lapply(census_years, function(y) {
  message("building ", y)
  build_year(y)
}))
write_csv(summary_table, file.path(derived_dir,
                                   paste0("dissimilarity_build_summary", variant_tag(), ".csv")))
print(as.data.frame(summary_table))
