# Residents who work, counting work done alongside housework or study.
#
# The proportional flow divides the commuting flows between two municipalities by the
# smaller of their two workforces, and the baseline counts as working only the residents
# whose labour-force status is "mainly working". Someone who works alongside housework or
# study is working too, and there are about eight million of them in every census year,
# a sixth again on top of the baseline count. Whether that reading of who is working
# changes the delineation is a question about the denominator on its own, so this builds
# the wider count while the flows stay on the narrower sample.
#
# The count comes from the attribute-level census tabulation under data/master of the
# shared project folder, which carries labour-force status and so allows the three
# working statuses to be added up. It reaches 1980 to 2015. The 2020 tabulation was
# delivered already aggregated into the two samples the pipeline reads, and the statuses
# inside the wider of them cannot be separated, so 2020 has no wider count.
#
# The municipality universe has to be built here rather than taken from the commuting
# matrix, because the tabulation keeps the wards of the ordinance-designated cities
# apart. The ranges below are the ones the shared folder's own Stata code collapses, and
# the twenty-three special wards of Tokyo are merged on top of them as everywhere else.
# The column order of the tabulation changes between census years, so the columns are
# taken by name.
#
# Writes validation/derived/denominator_with_side_work_<year>.csv, not committed.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

master_dir <- file.path(shared_root, "data/raw/commuteCensusData/data/master")
side_work_years <- census_years[census_years <= 2015]

# Labour-force statuses that count as working: mainly working, working alongside
# housework, working alongside study. Persons temporarily absent from a job are status 4
# and are left out, as they are from the baseline sample.
working_statuses <- 1:3

# Wards of an ordinance-designated city collapse into the city, as the lower end of each
# range. The Stata code in the shared folder applies exactly these ranges before it
# writes the commuting matrices the delineation reads.
ward_ranges <- tribble(
  ~from,  ~to,    ~city,
   1100,  1199,   1100,   # Sapporo
   4100,  4199,   4100,   # Sendai
  11100, 11199,  11100,   # Saitama
  12100, 12199,  12100,   # Chiba
  14100, 14129,  14100,   # Yokohama
  14130, 14149,  14130,   # Kawasaki
  14150, 14199,  14150,   # Sagamihara
  15100, 15199,  15100,   # Niigata
  22100, 22129,  22100,   # Shizuoka
  22130, 22199,  22130,   # Hamamatsu
  23100, 23199,  23100,   # Nagoya
  26100, 26199,  26100,   # Kyoto
  27100, 27139,  27100,   # Osaka
  27140, 27199,  27140,   # Sakai
  28100, 28199,  28100,   # Kobe
  33100, 33199,  33100,   # Okayama
  34100, 34199,  34100,   # Hiroshima
  40100, 40129,  40100,   # Kitakyushu
  40130, 40199,  40130,   # Fukuoka
  43100, 43199,  43100    # Kumamoto
)

#' Municipality code a residence is counted under, on the delineation's universe.
delineation_code <- function(code) {
  hit <- match(TRUE, code >= ward_ranges$from & code <= ward_ranges$to)
  city <- if (is.na(hit)) code else ward_ranges$city[hit]
  merge_tokyo_wards(sprintf("%05d", city))
}

build_year <- function(year) {
  raw <- suppressWarnings(read_csv(
    file.path(master_dir, sprintf("commute_%d_harmonized.csv", year)),
    col_select = c("living_mun", "work_status", "pop"),
    col_types = cols(living_mun = col_integer(), work_status = col_integer(),
                     pop = col_double())))

  counted <- raw %>%
    filter(!is.na(living_mun), work_status %in% working_statuses) %>%
    group_by(living_mun) %>%
    summarise(pop = sum(pop), .groups = "drop") %>%
    mutate(code = vapply(living_mun, delineation_code, character(1))) %>%
    group_by(code) %>%
    summarise(workers = sum(pop), .groups = "drop") %>%
    arrange(code)

  write_csv(counted, side_work_denominator_path(year))

  # The wider count has to hold every resident the baseline counts, in every
  # municipality, or the two are not nested and the comparison means nothing.
  narrow <- read_commuting(year) %>%
    group_by(i) %>%
    summarise(baseline = sum(pop), .groups = "drop")
  merged <- narrow %>% left_join(counted, by = c("i" = "code"))
  stopifnot(!any(is.na(merged$workers)), all(merged$workers >= merged$baseline))

  tibble(year = year, municipalities = nrow(counted), workers = sum(counted$workers),
         baseline_workers = sum(merged$baseline),
         ratio = sum(counted$workers) / sum(merged$baseline))
}

summary_table <- bind_rows(lapply(side_work_years, function(y) {
  message("counting ", y)
  build_year(y)
}))
write_csv(summary_table, file.path(derived_dir, "denominator_with_side_work_summary.csv"))
print(as.data.frame(summary_table))
