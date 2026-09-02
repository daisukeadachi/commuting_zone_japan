# The commuting zones of the people who work alongside housework.
#
# The baseline delineation is built on the flows of residents whose labour-force status
# is "mainly working". Some eight million people work alongside housework instead, and
# where they commute is a question about them: four in five work in the municipality they
# live in, against three in five of the mainly working, so their labour markets are
# smaller and a delineation built on their flows should show it.
#
# Two delineations are read off those flows, differing in what the proportional flow
# divides by. One divides by the baseline workforce, which isolates the change of
# numerator: everything but the flows is held at the baseline. The other divides by the
# workers the flows themselves count, which is the delineation of this group on its own
# terms and the one whose zones are worth looking at.
#
# Containment is measured on the flows of the group throughout, including for the
# baseline delineation. That last number is the practical one: it says what containment a
# user of the published zones gets for this population.
#
# Writes validation/output/numerator_comparison.csv.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

stopifnot(sample_definition == "WORK_MAIN", denominator == "reported")
comparison_years <- census_years[census_years <= 2015]

#' Containment of a partition on a given set of flows.
containment <- function(zone, flows) {
  f <- flows %>% mutate(same = zone[i] == zone[j])
  by_residence <- f %>%
    group_by(code = i) %>%
    summarise(residents = sum(pop), residents_in_zone = sum(pop[same]), .groups = "drop") %>%
    mutate(contained = residents_in_zone / residents, zone = zone[code])
  by_zone <- by_residence %>%
    group_by(zone) %>%
    summarise(residents = sum(residents), mean_contained = mean(contained), .groups = "drop")
  list(mean_contained = mean(by_zone$mean_contained),
       share_contained = sum(by_residence$residents_in_zone) / sum(by_residence$residents))
}

rows <- bind_rows(lapply(comparison_years, function(year) {
  message("comparing ", year)
  side_flows_all <- read_commuting(year, which = "SIDE_WORK")
  main_flows_all <- read_commuting(year, which = "WORK_MAIN")

  bind_rows(lapply(cutoff_anchors, function(cutoff) {
    read_zones <- function(sample, den) {
      old <- sample_definition
      assign("sample_definition", sample, envir = globalenv())
      path <- zone_path(year, cutoff, "constrained", den)
      assign("sample_definition", old, envir = globalenv())
      z <- read_csv(path, col_types = cols(code = col_character(), zone = col_integer()))
      setNames(z$zone, z$code)
    }
    baseline <- read_zones("WORK_MAIN", "reported")
    units <- names(baseline)
    own_denominator <- read_zones("SIDE_WORK", "reported")[units]
    baseline_denominator <- read_zones("SIDE_WORK", "mainly_working")[units]

    side <- side_flows_all %>% filter(i %in% units, j %in% units)
    main <- main_flows_all %>% filter(i %in% units, j %in% units)
    weight <- side %>% group_by(i) %>% summarise(w = sum(pop), .groups = "drop")
    weight <- setNames(weight$w, weight$i)[units]

    score <- jaccard(baseline, own_denominator)
    ct_baseline <- containment(baseline, side)
    ct_own <- containment(own_denominator, side)
    ct_held <- containment(baseline_denominator, side)

    tibble(
      year = year, cutoff = cutoff, municipalities = length(units),
      side_work_workers = sum(weight),
      zones_baseline = n_distinct(baseline),
      zones_side_work = n_distinct(own_denominator),
      zones_side_work_baseline_denominator = n_distinct(baseline_denominator),
      singletons_side_work = sum(table(own_denominator) == 1),
      singletons_side_work_baseline_denominator = sum(table(baseline_denominator) == 1),
      largest_zone_side_work = max(table(own_denominator)),
      mean_similarity_to_baseline = mean(score),
      weighted_similarity_to_baseline = weighted.mean(score, weight),
      mean_contained_baseline_zones = ct_baseline$mean_contained,
      mean_contained_side_work_zones = ct_own$mean_contained,
      mean_contained_baseline_denominator = ct_held$mean_contained,
      share_contained_baseline_zones = ct_baseline$share_contained,
      share_contained_side_work_zones = ct_own$share_contained,
      share_contained_mainly_working_flows = containment(baseline, main)$share_contained)
  }))
}))

write_csv(rows, file.path(output_dir, "numerator_comparison.csv"))
print(as.data.frame(rows))
