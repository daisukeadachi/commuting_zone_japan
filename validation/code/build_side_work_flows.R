# Commuting flows of the people who work alongside housework.
#
# The delineation is built on the flows of residents whose labour-force status is "mainly
# working". Beside them the census counts about eight million people whose status is work
# alongside housework, and where those people commute is a question about them rather
# than about the baseline: their journeys are shorter, and three in four of them work in
# the municipality they live in, against one in two of the mainly working. This builds
# their commuting matrix on its own so that a delineation can be run on it.
#
# The matrix is written in the layout of the ones the pipeline already reads, so that
# read_commuting takes it unchanged. That means the wards of an ordinance-designated city
# collapse into the city, which is what the shared folder's own Stata code does before it
# writes the matrices the delineation reads, and rows with no municipality on either side
# are dropped. The column order of the tabulation changes between census years, so the
# columns are taken by name.
#
# It reaches 1980 to 2015, the years the attribute-level tabulation covers. The 2020
# delivery arrived already aggregated into two samples and the statuses inside the wider
# of them cannot be separated.
#
# Writes validation/derived/SIDE_WORK/commute_<year>_harmonized.csv, not committed.

suppressMessages({
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")

master_dir <- file.path(shared_root, "data/raw/commuteCensusData/data/master")
flow_dir <- file.path(derived_dir, "SIDE_WORK")
dir.create(flow_dir, showWarnings = FALSE, recursive = TRUE)
side_work_years <- census_years[census_years <= 2015]

# Work alongside housework, the status this matrix isolates.
side_work_status <- 2L

# Wards of an ordinance-designated city collapse into the city, as the lower end of each
# range. These are the ranges the shared folder's Stata code applies.
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

#' Municipality code a ward is counted under.
city_code <- function(code) {
  hit <- match(TRUE, code >= ward_ranges$from & code <= ward_ranges$to)
  if (is.na(hit)) code else ward_ranges$city[hit]
}

build_year <- function(year) {
  raw <- suppressWarnings(read_csv(
    file.path(master_dir, sprintf("commute_%d_harmonized.csv", year)),
    col_select = c("living_mun", "commute_mun", "work_status", "pop"),
    col_types = cols(living_mun = col_integer(), commute_mun = col_integer(),
                     work_status = col_integer(), pop = col_double())))

  kept <- raw %>%
    filter(!is.na(living_mun), !is.na(commute_mun), work_status == side_work_status)
  lookup <- vapply(sort(unique(c(kept$living_mun, kept$commute_mun))), city_code, numeric(1))
  names(lookup) <- sort(unique(c(kept$living_mun, kept$commute_mun)))

  matrix_rows <- kept %>%
    transmute(living_mun = lookup[as.character(living_mun)],
              commute_mun = lookup[as.character(commute_mun)], pop) %>%
    group_by(living_mun, commute_mun) %>%
    summarise(pop = sum(pop), .groups = "drop") %>%
    group_by(living_mun) %>%
    mutate(year = year, tot_pop_living_mun = sum(pop)) %>%
    ungroup() %>%
    arrange(living_mun, commute_mun)

  write_csv(matrix_rows, commute_path(year, "harmonized", "SIDE_WORK"))

  tibble(year = year, origins = n_distinct(matrix_rows$living_mun),
         rows = nrow(matrix_rows), workers = sum(matrix_rows$pop),
         own_municipality_share =
           sum(matrix_rows$pop[matrix_rows$living_mun == matrix_rows$commute_mun]) /
           sum(matrix_rows$pop))
}

summary_table <- bind_rows(lapply(side_work_years, function(y) {
  message("building ", y)
  build_year(y)
}))
write_csv(summary_table, file.path(derived_dir, "side_work_flow_summary.csv"))
print(as.data.frame(summary_table))
