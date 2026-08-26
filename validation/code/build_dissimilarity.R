# Proportional-flow dissimilarity matrices, one per census year.
#
# For municipalities i and j the proportional flow is the sum of the two directed
# commuting flows divided by the smaller of the two resident labour forces, capped at
# 0.999, and the dissimilarity is one minus that. The cap is what the existing Japanese
# pipeline writes as a floor of 0.001 on the dissimilarity; the two are algebraically
# the same.
#
# Writes validation/derived/dissimilarity_<year>.rds and
# validation/derived/labour_force_<year>.csv. Neither is committed.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"), col_types = cols(code = col_character()))
in_scope <- scope$code[scope$in_scope]
dir.create(derived_dir, showWarnings = FALSE, recursive = TRUE)

build_year <- function(year) {
  flows <- read_commuting(year)

  reported <- flows %>% group_by(i) %>% summarise(rlf_reported = sum(pop), .groups = "drop")
  units <- sort(intersect(unique(flows$i), in_scope))
  kept <- flows %>% filter(i %in% units, j %in% units)

  mat <- matrix(0, nrow = length(units), ncol = length(units), dimnames = list(units, units))
  mat[cbind(match(kept$i, units), match(kept$j, units))] <- kept$pop

  # Resident labour force is the row sum over destinations inside the scope, following
  # the source paper, which computes it from the square matrix it clusters on.
  rlf <- rowSums(mat)
  pair_min <- pmin(matrix(rlf, length(units), length(units), byrow = FALSE),
                   matrix(rlf, length(units), length(units), byrow = TRUE))
  prop <- (mat + t(mat)) / pair_min
  prop[prop >= 1 - dissimilarity_floor] <- 1 - dissimilarity_floor
  dissim <- 1 - prop
  diag(dissim) <- 0

  saveRDS(as.dist(dissim), file.path(derived_dir, sprintf("dissimilarity_%d.rds", year)))

  labour <- tibble(code = units, rlf_in_scope = rlf) %>%
    left_join(reported, by = c("code" = "i")) %>%
    mutate(dropped_to_out_of_scope = rlf_reported - rlf_in_scope)
  write_csv(labour, file.path(derived_dir, sprintf("labour_force_%d.csv", year)))

  tibble(
    year = year,
    municipalities_reported = length(unique(flows$i)),
    municipalities_in_scope = length(units),
    labour_force_in_scope = sum(rlf),
    labour_force_reported = sum(labour$rlf_reported),
    share_lost_to_out_of_scope_destinations = 1 - sum(rlf) / sum(labour$rlf_reported)
  )
}

summary_table <- bind_rows(lapply(census_years, function(y) {
  message("building ", y)
  build_year(y)
}))
write_csv(summary_table, file.path(derived_dir, "dissimilarity_build_summary.csv"))
print(as.data.frame(summary_table))
