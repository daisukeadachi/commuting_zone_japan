# Containment block of the fit statistics, the counterpart of the Containment half of
# Table 2 of the source paper, for every census year at both cutoff anchors.
#
# Containment of a municipality is the share of its residents who work inside its own
# commuting zone. Work containment is the share of the jobs located in the municipality
# that are filled by residents of that same zone. Both are reported unweighted across
# municipalities, as the source paper does, and weighted by resident labour force, which
# issue #11 asks for alongside.
#
# Writes validation/output/containment_table.csv, validation/output/containment_by_zone.csv
# and validation/output/containment_by_municipality.csv.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"), col_types = cols(code = col_character())) %>%
  filter(in_scope)

containment <- function(year, cutoff) {
  zones <- read_csv(file.path(derived_dir, sprintf("zones_%d_cut%s.csv", year, format(cutoff, nsmall = 3))),
                    col_types = cols(code = col_character(), zone = col_integer()))
  flows <- read_commuting(year) %>% filter(i %in% zones$code, j %in% zones$code)

  zone_of <- setNames(zones$zone, zones$code)
  flows <- flows %>% mutate(zone_i = zone_of[i], zone_j = zone_of[j], same = zone_i == zone_j)

  by_residence <- flows %>%
    group_by(code = i) %>%
    summarise(residents = sum(pop), residents_in_zone = sum(pop[same]), .groups = "drop") %>%
    mutate(contained = residents_in_zone / residents)

  by_workplace <- flows %>%
    group_by(code = j) %>%
    summarise(jobs = sum(pop), jobs_from_zone = sum(pop[same]), .groups = "drop") %>%
    mutate(work_contained = jobs_from_zone / jobs)

  municipalities <- zones %>%
    left_join(by_residence, by = "code") %>%
    left_join(by_workplace, by = "code")

  by_zone <- municipalities %>%
    group_by(zone) %>%
    summarise(
      municipalities = n(),
      residents = sum(residents),
      min_contained = min(contained),
      mean_contained = mean(contained),
      labour_weighted_contained = sum(residents_in_zone) / sum(residents),
      mean_work_contained = mean(work_contained, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = year, cutoff = cutoff, .before = 1)

  list(
    by_municipality = municipalities %>%
      transmute(year = year, cutoff = cutoff, code, zone, residents,
                contained, jobs, work_contained),
    row = tibble(
      year = year,
      cutoff = cutoff,
      commuting_zones = nrow(by_zone),
      min_contained = min(by_zone$mean_contained),
      mean_contained = mean(by_zone$mean_contained),
      labour_weighted_mean_contained = weighted.mean(by_zone$mean_contained, by_zone$residents),
      share_of_labour_force_contained = sum(municipalities$residents_in_zone) / sum(municipalities$residents),
      min_work_contained = min(by_zone$mean_work_contained),
      mean_work_contained = mean(by_zone$mean_work_contained),
      lowest_containment_zone = by_zone$zone[which.min(by_zone$mean_contained)]
    ),
    by_zone = by_zone
  )
}

results <- list()
for (year in census_years) {
  for (cutoff in cutoff_anchors) {
    message("containment ", year, " at ", cutoff)
    results[[length(results) + 1]] <- containment(year, cutoff)
  }
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(bind_rows(lapply(results, `[[`, "row")), file.path(output_dir, "containment_table.csv"))
write_csv(bind_rows(lapply(results, `[[`, "by_zone")), file.path(output_dir, "containment_by_zone.csv"))
write_csv(bind_rows(lapply(results, `[[`, "by_municipality")),
          file.path(output_dir, "containment_by_municipality.csv"))
print(as.data.frame(bind_rows(lapply(results, `[[`, "row"))))
