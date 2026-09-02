# Shared paths, coordinate systems and parameters for the Fowler (2024) replication.
# Sourced by every other script under validation/code.

# Root of the shared project folder. The commuting matrices and the boundary layer
# live here rather than in the repository; see validation/notes/handoff.md.
shared_root <- "C:/Users/au698627/Dropbox/projects/Kawaguchi_Saito/CommutingZone/adachi/clustering"

commute_dir <- file.path(shared_root, "data/raw/commuteCensusData/data/use")
boundary_shp <- file.path(shared_root, "data/raw/mmm/shapefiles/mmm20151001_ku_aggregate/mmm20151001.shp")

# The 2020 matrices arrived later than the rest and sit in their own shared folder, one
# subfolder per labour-force sample as under commute_dir. Only 2020 is there: 1980 to
# 2015 exist under commute_dir alone, and the 2020 matrix on "mainly working" exists in
# both places, where the two files hold the same rows in a different order. Reading 2020
# from here rather than from commute_dir therefore leaves the delineation unchanged, and
# it is the only way to reach the sample that also counts secondary and student jobs,
# which was delivered for 2020 here and nowhere else.
census2020_dir <- "C:/Users/au698627/Dropbox/projects/CZ_RA/census2020"
census2020_year <- 2020L

# Repository paths. Derived objects are large and are not committed.
data_dir <- "validation/data"
derived_dir <- "validation/derived"
output_dir <- "validation/output"

# Which delineation the diagnostics are computed on. The contiguity-constrained
# delineation of Guenard and Legendre (2022) is the baseline and is what every committed
# table and figure reports under its plain name. The unconstrained delineation is
# computed alongside it: set the environment variable CZ_DELINEATION to "unconstrained"
# before running, having built its trees first with build_clusters.R. Under that setting
# every script reads the unconstrained zones and writes its tables and figures under
# names carrying the delineation, so the two runs stand beside each other and the
# appendix can show what the constraint removes.
delineation <- Sys.getenv("CZ_DELINEATION", "constrained")
stopifnot(delineation %in% c("unconstrained", "constrained"))

# Which labour-force sample the commuting matrix is drawn from. WORK_MAIN is the baseline
# and holds persons whose labour-force status is "mainly working". WORK_ALL widens it to
# persons working alongside housework or study and to persons temporarily absent from a
# job, which adds about a fifth to the counted labour force. Set the environment variable
# CZ_SAMPLE to "WORK_ALL" to run on the wider sample; every derived object and every
# table and figure then carries the sample in its name, so the two runs stand beside each
# other and neither overwrites the baseline.
#
# SIDE_WORK is the flows of the eight million people who work alongside housework, on
# their own, which is what a delineation of that group's commuting zones needs.
# build_side_work_flows.R builds it into the repository's derived folder rather than the
# shared one, and it reaches 1980 to 2015 for the same reason the wider denominator does.
sample_definition <- Sys.getenv("CZ_SAMPLE", "WORK_MAIN")
stopifnot(sample_definition %in% c("WORK_MAIN", "WORK_ALL", "SIDE_WORK"))

#' File-name tag for a labour-force sample, empty under the baseline.
sample_tag <- function(which = sample_definition) {
  if (which == "WORK_MAIN") "" else paste0("_", tolower(which))
}

# Which count of workers the proportional flow divides by. The source paper writes the
# denominator as the total workforce of each county and computes it as the row sum of the
# commuting flow matrix, over every destination the census records; "reported" is that
# quantity and is the baseline. "in_scope" sums only over the destinations inside the
# geographic scope the clustering runs on, which is what the pipeline did before and
# differs from the baseline by less than a thousandth of the workforce in every year.
# "with_side_work" widens the count to residents who work alongside housework or study
# while leaving the flows themselves on the "mainly working" sample, so that what a wider
# reading of who is working does to the delineation can be read off the denominator
# alone; build_side_work_denominator.R builds it, and it exists for 1980 to 2015 only.
# "mainly_working" holds the count at the baseline whatever sample the flows are drawn
# from, which is what isolates a change of numerator: a delineation built on the flows of
# one group, divided by the workforce the baseline divides by, differs from the baseline
# only in its flows. Under the baseline sample the two are the same number.
# Set the environment variable CZ_DENOMINATOR to choose; away from the baseline every
# derived object and every table and figure carries the choice in its name.
denominator <- Sys.getenv("CZ_DENOMINATOR", "reported")
stopifnot(denominator %in% c("reported", "in_scope", "with_side_work", "mainly_working"))

#' File-name tag for a denominator, empty under the baseline.
denominator_tag <- function(which = denominator) {
  if (which == "reported") "" else paste0("_", which)
}

#' File-name tag for everything the delineation is conditioned on bar the contiguity
#' constraint, which the paths carry separately.
variant_tag <- function(den = denominator) paste0(sample_tag(), denominator_tag(den))

#' Workers resident in each municipality, the denominator of the proportional flow.
#'
#' @param flows the census year's commuting matrix, on the merged municipality universe
#' @param units the municipalities the delineation runs on
#' @param year census year, read only when the count comes from outside the matrix
#' @return named numeric vector in the order of units
resident_labour_force <- function(flows, units, year) {
  count <- switch(denominator,
    reported = flows |> dplyr::group_by(i) |>
      dplyr::summarise(w = sum(pop), .groups = "drop"),
    in_scope = flows |> dplyr::filter(j %in% units) |> dplyr::group_by(i) |>
      dplyr::summarise(w = sum(pop), .groups = "drop"),
    with_side_work = suppressMessages(readr::read_csv(
      side_work_denominator_path(year),
      col_types = readr::cols(code = readr::col_character()))) |>
      dplyr::transmute(i = code, w = workers),
    mainly_working = read_commuting(year, which = "WORK_MAIN") |> dplyr::group_by(i) |>
      dplyr::summarise(w = sum(pop), .groups = "drop"))
  out <- setNames(count$w, count$i)[units]
  stopifnot(!any(is.na(out)), all(out > 0))
  out
}

#' Path of the count of residents working alongside housework or study.
side_work_denominator_path <- function(year) {
  file.path(derived_dir, sprintf("denominator_with_side_work_%d.csv", year))
}

#' Path of one census year's commuting matrix.
#'
#' @param year census year
#' @param code_type "harmonized" or "original"
#' @param which labour-force sample; defaults to the selected one
commute_path <- function(year, code_type = "harmonized", which = sample_definition) {
  root <- if (which == "SIDE_WORK") derived_dir
          else if (year == census2020_year) census2020_dir else commute_dir
  file.path(root, which, sprintf("commute_%d_%s.csv", year, code_type))
}

#' Path of a derived object that depends on the sample but not on the delineation.
#'
#' @param stem file name up to the year, without the sample tag
#' @param year census year
#' @param ext file extension, including the dot
derived_path <- function(stem, year, ext = ".rds", den = denominator) {
  file.path(derived_dir, sprintf("%s%s_%d%s", stem, variant_tag(den), year, ext))
}

#' Path of a saved agglomeration.
#'
#' @param year census year
#' @param which "unconstrained" or "constrained"; defaults to the selected delineation
tree_path <- function(year, which = delineation, den = denominator) {
  derived_path(if (which == "constrained") "constrained_hclust" else "hclust", year, den = den)
}

# The cutoff as it appears in a file name, at three decimals.
cut_label <- function(cutoff) format(cutoff, nsmall = 3)

#' Path of a full-coverage zone assignment, the one that carries the offshore islands.
full_zone_path <- function(year, cutoff) {
  file.path(derived_dir, sprintf("zones_full_%d_cut%s%s.csv", year, cut_label(cutoff),
                                 variant_tag()))
}

#' Path of a zone assignment written by the build step.
#'
#' @param year census year
#' @param cutoff tree height the assignment was cut at
#' @param which "unconstrained" or "constrained"; defaults to the selected delineation
#' @param den denominator the dissimilarity was built with; defaults to the selected one
zone_path <- function(year, cutoff, which = delineation, den = denominator) {
  prefix <- if (which == "constrained") "zones_constrained" else "zones"
  file.path(derived_dir, sprintf("%s%s_%d_cut%s.csv", prefix, variant_tag(den), year,
                                 cut_label(cutoff)))
}

#' Destination of a diagnostic table.
#'
#' Away from the baseline the delineation and the labour-force sample are written into
#' the file name, which is what keeps an unconstrained or a wider-sample run from
#' overwriting the baseline tables.
#'
#' @param name file name the table carries under the baseline
#' @param which "unconstrained" or "constrained"; defaults to the selected delineation
output_path <- function(name, which = delineation) {
  tag <- paste0(if (which == "constrained") "" else "_unconstrained", variant_tag())
  if (tag == "") return(file.path(output_dir, name))
  file.path(output_dir, sub("([.][^.]+)$", paste0(tag, "\\1"), name))
}

#' Directory the figures are written to, one per delineation, sample and denominator.
figure_path <- function(which = delineation) {
  stem <- if (which == "unconstrained") "figures_unconstrained" else "figures"
  file.path(output_dir, paste0(stem, variant_tag()))
}

#' Zones obtained by stopping an agglomeration at a cutoff.
#'
#' The merges are accepted in the order the algorithm made them, up to the first one
#' whose height exceeds the cutoff. This matters only for a constrained tree, whose
#' heights need not increase: when a cluster is adjacent to one half of a merging pair
#' and not to the other, only the adjacent half is known to lie above the merge height,
#' and the average of the two can fall below it. A later merge cheaper than the cutoff is
#' nevertheless refused, because it became available only through the merge that stopped
#' the agglomeration. Where the heights do increase this is exactly cutree.
#'
#' @param tree an object of class hclust or constr.hclust
#' @param cutoff tree height at which to stop
#' @return named integer vector, one zone label per unit
cut_tree_at_height <- function(tree, cutoff) {
  n <- length(tree$labels)
  parent <- seq_len(n)
  root <- function(i) { while (parent[i] != i) i <- parent[i]; i }
  above <- which(tree$height > cutoff)
  accepted <- if (length(above)) seq_len(above[1] - 1L) else seq_len(nrow(tree$merge))
  representative <- integer(nrow(tree$merge))
  for (s in accepted) {
    side <- vapply(1:2, function(j) {
      if (tree$merge[s, j] < 0) -tree$merge[s, j] else representative[tree$merge[s, j]]
    }, integer(1))
    a <- root(side[1]); b <- root(side[2])
    parent[b] <- a
    representative[s] <- a
  }
  roots <- vapply(seq_len(n), root, integer(1))
  setNames(match(roots, unique(roots)), tree$labels)
}

# Trees are read from disk once per session. The cutoff sweep asks for dozens of cuts of
# the same year, and reading the saved tree each time dominates the cost of cutting it.
.tree_cache <- new.env(parent = emptyenv())

load_tree <- function(year) {
  key <- sprintf("%s_%s_%s_%d", delineation, sample_definition, denominator, year)
  if (!exists(key, envir = .tree_cache)) {
    assign(key, readRDS(tree_path(year)), envir = .tree_cache)
  }
  get(key, envir = .tree_cache)
}

#' Municipalities the delineation covers in one census year.
delineation_units <- function(year) {
  tree <- load_tree(year)
  if (delineation == "constrained") names(tree$membership) else tree$labels
}

#' Zone assignment for one year at any cutoff, under the selected delineation.
#'
#' The constrained delineation is clustered separately on each connected component of the
#' adjacency graph, so its assignment is assembled component by component with the zone
#' labels offset. A component holding one municipality is its own zone.
#'
#' @return named integer vector, one zone label per municipality
zone_assignment <- function(year, cutoff) {
  tree <- load_tree(year)
  if (delineation != "constrained") return(cutree(tree, h = cutoff))
  offset <- 0
  out <- integer(0)
  for (k in names(tree$trees)) {
    members <- names(tree$membership)[tree$membership == as.integer(k)]
    zone <- if (is.null(tree$trees[[k]])) setNames(1L, members) else
      cut_tree_at_height(tree$trees[[k]], cutoff)
    out <- c(out, zone + offset)
    offset <- offset + length(unique(zone))
  }
  out[order(names(out))]
}

#' Jaccard similarity of two partitions, municipality by municipality.
#'
#' For each municipality it is the size of the intersection of the two sets of
#' municipalities sharing its zone, over the size of their union, each set including the
#' municipality itself. It is read from the cross-tabulation of the two partitions: the
#' intersection is the count of municipalities sharing both zones, and the union follows
#' from the two zone sizes.
jaccard <- function(zone_a, zone_b) {
  counts <- table(zone_a, zone_b)
  size_a <- rowSums(counts)
  size_b <- colSums(counts)
  both <- counts[cbind(match(zone_a, rownames(counts)), match(zone_b, colnames(counts)))]
  both / (size_a[match(zone_a, names(size_a))] + size_b[match(zone_b, names(size_b))] - both)
}

#' Municipalities that share their commuting zone with none of their neighbours.
#'
#' This is the source paper's non-contiguity test. A municipality alone in its zone counts
#' as contiguous by construction. A neighbour absent from the delineation, which happens
#' where a municipality is missing from a census year, counts as not sharing the zone, so
#' a municipality all of whose neighbours are absent reads as detached.
#'
#' @param zone named vector of zone labels, one per municipality
#' @param neighbours list of adjacent municipality codes, keyed by municipality code
#' @return logical vector in the order of names(zone)
detached_municipalities <- function(zone, neighbours) {
  sizes <- table(zone)
  vapply(names(zone), function(m) {
    if (sizes[as.character(zone[m])] == 1) return(FALSE)
    !any(zone[neighbours[[m]]] == zone[m], na.rm = TRUE)
  }, logical(1))
}

# Japan-wide equal-area conic projection, the counterpart of the NAD83 / Conus Albers
# system (EPSG:5070) that Fowler (2024) uses before computing area and compactness.
# No EPSG code can serve: every Japanese projected system in the registry is
# Transverse Mercator and zone-based. See the comment on issue #11.
crs_equal_area <- "+proj=aea +lat_1=30 +lat_2=42 +lat_0=36 +lon_0=136 +ellps=GRS80 +units=m +no_defs"

# Census years available in the commuting matrices. The wider count of workers is built
# from the attribute-level tabulation, whose last year is 2015, so a run on that
# denominator stops there.
census_years <- seq(1980, 2020, by = 5)
if (denominator == "with_side_work" || sample_definition == "SIDE_WORK") {
  census_years <- census_years[census_years <= 2015]
}

# Dissimilarity floor of the existing pipeline. Algebraically identical to the cap of
# 0.999 that Fowler places on the proportional flow.
dissimilarity_floor <- 0.001

# The baseline cutoff, the value the source paper uses. It is the one the prose, the
# slides and the headline figures report. Every table and figure carries both anchors,
# so the alternative value always sits alongside the baseline rather than replacing it.
baseline_cutoff <- 0.977

# Cutoff anchors for the earlier year of each comparison pair. The order fixes the row
# order of the output tables and is left as it is, so that a rebuild reproduces the
# committed files unchanged.
cutoff_anchors <- c(0.980, baseline_cutoff)

# Baseline comparison pairs. The first is the headline; the other two are
# pre-pandemic controls.
baseline_pairs <- list(c(2010, 2020), c(2010, 2015), c(2005, 2015))

# Decade pairs reported in the appendix for completeness.
appendix_pairs <- list(c(1980, 1990), c(1990, 2000), c(2000, 2010), c(2010, 2020))

# National maps carry Okinawa far to the southwest of the main islands, which stretches
# the frame and leaves most of it empty. The source paper meets the same problem with
# Alaska and Hawaii and answers it by repositioning them; this does the same for Okinawa,
# moving the block into the empty sea northwest of the main islands and returning the
# rectangle to draw around it.
inset_okinawa <- function(layer, okinawa_codes, margin = 60000) {
  is_okinawa <- layer$code %in% okinawa_codes
  if (!any(is_okinawa)) return(list(layer = layer, frame = NULL))
  main_box <- sf::st_bbox(layer[!is_okinawa, ])
  okinawa_box <- sf::st_bbox(layer[is_okinawa, ])
  offset <- c(
    unname(main_box["xmin"] + margin - okinawa_box["xmin"]),
    unname(main_box["ymax"] - margin - okinawa_box["ymax"])
  )
  moved <- sf::st_geometry(layer)[is_okinawa] + offset
  geometry <- sf::st_geometry(layer)
  geometry[is_okinawa] <- moved
  sf::st_geometry(layer) <- geometry
  sf::st_crs(layer) <- crs_equal_area
  frame <- sf::st_as_sfc(sf::st_bbox(
    sf::st_buffer(sf::st_union(layer[is_okinawa, ]), margin / 3)
  ))
  sf::st_crs(frame) <- crs_equal_area
  list(layer = layer, frame = frame)
}

# The twenty-three special wards of Tokyo are treated as one municipality, as the
# ordinance-designated cities are. The Urban Employment Area delineation already uses
# this unit under the code 13100, so the Core measures line up with it without any
# further mapping. Every layer that carries municipality codes goes through
# merge_tokyo_wards before it is used.
tokyo_ward_codes <- sprintf("131%02d", 1:23)
tokyo_merged_code <- "13100"
tokyo_merged_name <- "東京都特別区部"

merge_tokyo_wards <- function(codes) {
  ifelse(codes %in% tokyo_ward_codes, tokyo_merged_code, codes)
}

# Commuting flows on the merged universe. Reading them anywhere else risks a layer that
# still carries the twenty-three ward codes.
read_commuting <- function(year, code_type = "harmonized", which = sample_definition) {
  raw <- suppressMessages(readr::read_csv(commute_path(year, code_type, which),
                                          show_col_types = FALSE))
  raw |>
    dplyr::transmute(
      i = merge_tokyo_wards(sprintf("%05d", as.integer(living_mun))),
      j = merge_tokyo_wards(sprintf("%05d", as.integer(commute_mun))),
      pop
    ) |>
    dplyr::group_by(i, j) |>
    dplyr::summarise(pop = sum(pop), .groups = "drop")
}

# The boundary layer on the same municipality universe as everything else. Reading the
# shapefile directly leaves the twenty-three ward codes in place; they then fail the
# scope filter and the wards vanish from every map and from every area computed off one.
read_boundaries <- function(codes = NULL) {
  layer <- sf::st_read(boundary_shp, quiet = TRUE, options = "ENCODING=CP932")
  layer <- sf::st_make_valid(layer)
  layer$code <- merge_tokyo_wards(sprintf("%05d", as.integer(as.character(layer$JISCODE))))
  layer <- layer[, "code"]
  layer <- layer |>
    dplyr::group_by(code) |>
    dplyr::summarise(.groups = "drop") |>
    sf::st_make_valid()
  if (!is.null(codes)) layer <- layer[layer$code %in% codes, ]
  sf::st_transform(layer, crs_equal_area)
}
