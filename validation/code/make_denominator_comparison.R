# The delineation under a wider count of who is working, set beside the baseline.
#
# The proportional flow divides the commuting flows between two municipalities by the
# smaller of their two workforces. The baseline counts as working only the residents
# whose labour-force status is "mainly working"; widening the count to residents who work
# alongside housework or study adds about a fifth to it, and a larger denominator makes
# every link weaker at an unchanged cutoff. The flows in the numerator stay on the
# narrower sample here, so what the comparison isolates is the denominator.
#
# Reported for every census year from 1980 to 2015 and both cutoff anchors: the zone
# counts on each side, the municipality-level Jaccard similarity between the two
# partitions, the labour force sitting in a zone whose membership changes, and
# containment on each side. Containment is the share of a municipality's residents who
# work inside their own zone, and it is computed from the same flows on both sides, so it
# says which delineation fits the commuting the baseline sample records.
#
# Writes validation/output/denominator_comparison.csv and
# validation/output/denominator_movers.csv.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

stopifnot(denominator == "reported")
wider <- "with_side_work"
comparison_years <- census_years[census_years <= 2015]

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"),
                  col_types = cols(code = col_character())) %>% filter(in_scope)

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

rows <- bind_rows(lapply(comparison_years, function(year) {
  message("comparing ", year)
  labour <- read_csv(derived_path("labour_force", year, ".csv"),
                     col_types = cols(code = col_character()))
  counts <- read_csv(side_work_denominator_path(year),
                     col_types = cols(code = col_character()))
  all_flows <- read_commuting(year)

  bind_rows(lapply(cutoff_anchors, function(cutoff) {
    read_zones <- function(den) {
      z <- read_csv(zone_path(year, cutoff, "constrained", den),
                    col_types = cols(code = col_character(), zone = col_integer()))
      setNames(z$zone, z$code)
    }
    narrow <- read_zones(denominator)
    units <- names(narrow)
    wide <- read_zones(wider)[units]
    weight <- labour$denominator[match(units, labour$code)]
    flows <- all_flows %>% filter(i %in% units, j %in% units)

    score <- jaccard(narrow, wide)
    moved <- score < 1
    ct_narrow <- containment(narrow, flows)
    ct_wide <- containment(wide, flows)

    if (any(moved)) {
      # A municipality whose zone membership changes sits in a zone of one size under the
      # baseline and another under the wider count, and the share of its commuters
      # reaching each says which grouping its own flows support.
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
        workers_baseline = weight[moved],
        workers_with_side_work = counts$workers[match(units[moved], counts$code)],
        similarity = unname(score[moved]),
        baseline_zone_size = as.integer(table(narrow)[as.character(narrow[moved])]),
        wider_zone_size = as.integer(table(wide)[as.character(wide[moved])]),
        share_commuting_to_baseline_zone = unname(share_to(narrow)),
        share_commuting_to_wider_zone = unname(share_to(wide)))
    }

    tibble(
      year = year, cutoff = cutoff, municipalities = length(units),
      zones_baseline = n_distinct(narrow), zones_with_side_work = n_distinct(wide),
      workers_baseline = sum(weight),
      workers_with_side_work = sum(counts$workers[match(units, counts$code)]),
      mean_similarity = mean(score),
      labour_weighted_similarity = weighted.mean(score, weight),
      municipalities_in_a_changed_zone = sum(moved),
      share_of_labour_force_in_a_changed_zone = sum(weight[moved]) / sum(weight),
      mean_contained_baseline = ct_narrow$mean_contained,
      mean_contained_with_side_work = ct_wide$mean_contained,
      labour_weighted_contained_baseline = ct_narrow$labour_weighted_mean_contained,
      labour_weighted_contained_with_side_work = ct_wide$labour_weighted_mean_contained,
      share_contained_baseline = ct_narrow$share_of_labour_force_contained,
      share_contained_with_side_work = ct_wide$share_of_labour_force_contained)
  }))
}))

write_csv(rows, file.path(output_dir, "denominator_comparison.csv"))
write_csv(bind_rows(movers), file.path(output_dir, "denominator_movers.csv"))
print(as.data.frame(rows))
