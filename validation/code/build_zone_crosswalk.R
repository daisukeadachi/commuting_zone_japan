# Commuting zones attached to the municipality codes of every census date.
#
# A researcher pooling several censuses wants one set of commuting zones held fixed
# across years, and the natural choice is the delineation of the earliest year in their
# sample. What they have is a panel keyed by year and municipality code, and the codes
# move: municipalities merge, and a town incorporated as a city takes a new code. This
# writes the table that joins the two, one row per municipality per census date, giving
# the zone that municipality falls into under each year's delineation.
#
# The join is exact rather than approximate. Every delineation is built on commuting
# matrices already harmonized onto the codes in force on 1 October 2015, so each census
# date's code maps forward onto one 2015 unit and therefore onto one zone of any
# delineation. A municipality formed by a merger cannot straddle two zones of an earlier
# year's delineation, because the harmonization happens before the clustering rather
# than after it.
#
# The zones are the full-coverage ones, so the offshore islands are carried too and the
# table reaches the whole census population.
#
# One municipality genuinely splits. Kamikuishiki in Yamanashi, code 19341, was divided
# in 2006 between Kofu and Fujikawaguchiko, and it appears in the table twice, once per
# successor, with the share of its area in the weight column. The largest_share column
# marks the successor holding the larger share, for a user who needs one row per
# municipality. Every other municipality carries a single row with a weight of one.
#
# Writes validation/output/commuting_zone_crosswalk_cut<cutoff>.csv, one file per cutoff,
# and validation/output/commuting_zone_crosswalk_coverage.csv.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

crosswalk_dir <- file.path(data_dir, "crosswalk")

# Census dates whose crosswalk is written with the earlier date as the base, so the
# municipality's own code is the first of the pair, and those written from 2015 forward,
# where it is the second.
years_before_base <- c(1980L, 1985L, 1990L, 1995L, 2000L, 2005L, 2010L)
years_after_base <- c(2020L, 2025L)

#' Read one crosswalk file.
#'
#' The files carry a trailing empty field on every data row, which readr reports and
#' then discards; the warning says nothing about the twelve columns that matter.
read_codelist <- function(path) {
  suppressWarnings(read_csv(path, locale = locale(encoding = "CP932"),
                            col_types = cols(.default = col_character())))
}

#' Municipality name and the county that holds it, from one side of a crosswalk.
#'
#' The layout puts a county in the county field and the municipality in the municipality
#' field, except for a designated city or a Tokyo ward, which sits in the county field
#' alone.
naming <- function(gun, muni) {
  gun <- coalesce(gun, "")
  muni <- coalesce(muni, "")
  tibble(gun = ifelse(muni == "", "", gun), muni_name = ifelse(muni == "", gun, muni))
}

records <- bind_rows(
  lapply(years_before_base, function(year) {
    x <- read_codelist(file.path(crosswalk_dir,
                                 sprintf("codelist_%d1001and20151001.csv", year)))
    bind_cols(
      tibble(census_year = year, code = x$JISCODE1, prefecture = x$PNAME1),
      naming(x$GNAME1, x$CNAME1),
      tibble(weight = as.numeric(x$WEIGHT), code_2015 = x$JISCODE2))
  }),
  local({
    x <- read_codelist(file.path(crosswalk_dir, "codelist_20151001and20201001.csv"))
    bind_cols(
      tibble(census_year = 2015L, code = x$JISCODE1, prefecture = x$PNAME1),
      naming(x$GNAME1, x$CNAME1),
      tibble(weight = 1, code_2015 = x$JISCODE1))
  }),
  lapply(years_after_base, function(year) {
    x <- read_codelist(file.path(crosswalk_dir,
                                 sprintf("codelist_20151001and%d1001.csv", year)))
    bind_cols(
      tibble(census_year = year, code = x$JISCODE2, prefecture = x$PNAME2),
      naming(x$GNAME2, x$CNAME2),
      tibble(weight = 1, code_2015 = x$JISCODE1))
  })
) %>%
  group_by(census_year, code) %>%
  mutate(largest_share = weight == max(weight)) %>%
  ungroup() %>%
  arrange(census_year, code, desc(weight))

scope <- read_csv(file.path(data_dir, "municipality_scope.csv"),
                  col_types = cols(code = col_character()))
in_scope <- scope$code[scope$in_scope]

# The delineation is built on the universe that treats the twenty-three special wards of
# Tokyo as one municipality, so a ward's own 2015 code has to be mapped onto it before a
# zone can be looked up. The ward keeps its own code in the census-date column, which is
# the column a user joins on.
records <- records %>%
  mutate(delineation_unit = merge_tokyo_wards(code_2015),
         offshore_island = !delineation_unit %in% in_scope)

for (cutoff in cutoff_anchors) {
  table <- records
  for (anchor in census_years) {
    zones <- read_csv(file.path(derived_dir,
                                sprintf("zones_full_%d_cut%s.csv", anchor, cut_label(cutoff))),
                      col_types = cols(code = col_character(), zone = col_integer()))
    table[[sprintf("zone_%d", anchor)]] <-
      setNames(zones$zone, zones$code)[table$delineation_unit]
  }
  table <- table %>% select(-delineation_unit)
  write_csv(table, output_path(sprintf("commuting_zone_crosswalk_cut%s.csv", cut_label(cutoff))))
  message("wrote the crosswalk at cutoff ", cut_label(cutoff), ": ", nrow(table), " rows")
}

# Coverage, at the baseline cutoff. A municipality has no zone under an anchor year when
# the 2015 unit it falls into is absent from that year's commuting matrix, which happens
# for the Northern Territories, where no census is taken, and for the municipalities
# under evacuation orders.
coverage <- read_csv(output_path(sprintf("commuting_zone_crosswalk_cut%s.csv",
                                         cut_label(baseline_cutoff))),
                     col_types = cols(.default = col_guess(), code = col_character(),
                                      code_2015 = col_character())) %>%
  group_by(census_year) %>%
  summarise(rows = n(),
            municipalities = n_distinct(code),
            offshore_island_municipalities = n_distinct(code[offshore_island]),
            with_a_zone_under_1980 = sum(!is.na(zone_1980) & largest_share),
            with_a_zone_under_2020 = sum(!is.na(zone_2020) & largest_share),
            .groups = "drop")
write_csv(coverage, output_path("commuting_zone_crosswalk_coverage.csv"))
print(as.data.frame(coverage))
