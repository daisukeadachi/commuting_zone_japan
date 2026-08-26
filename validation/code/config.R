# Shared paths, coordinate systems and parameters for the Fowler (2024) replication.
# Sourced by every other script under validation/code.

# Root of the shared project folder. The commuting matrices and the boundary layer
# live here rather than in the repository; see validation/notes/handoff.md.
shared_root <- "C:/Users/au698627/Dropbox/projects/Kawaguchi_Saito/CommutingZone/adachi/clustering"

commute_dir <- file.path(shared_root, "data/raw/commuteCensusData/data/use/WORK_MAIN")
boundary_shp <- file.path(shared_root, "data/raw/mmm/shapefiles/mmm20151001_ku_aggregate/mmm20151001.shp")

# Repository paths. Derived objects are large and are not committed.
data_dir <- "validation/data"
derived_dir <- "validation/derived"
output_dir <- "validation/output"

# Japan-wide equal-area conic projection, the counterpart of the NAD83 / Conus Albers
# system (EPSG:5070) that Fowler (2024) uses before computing area and compactness.
# No EPSG code can serve: every Japanese projected system in the registry is
# Transverse Mercator and zone-based. See the comment on issue #11.
crs_equal_area <- "+proj=aea +lat_1=30 +lat_2=42 +lat_0=36 +lon_0=136 +ellps=GRS80 +units=m +no_defs"

# Census years available in the commuting matrices.
census_years <- seq(1980, 2020, by = 5)

# Dissimilarity floor of the existing pipeline. Algebraically identical to the cap of
# 0.999 that Fowler places on the proportional flow.
dissimilarity_floor <- 0.001

# Cutoff anchors for the earlier year of each comparison pair.
cutoff_anchors <- c(0.980, 0.977)

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
read_commuting <- function(year, code_type = "harmonized") {
  path <- file.path(commute_dir, sprintf("commute_%d_%s.csv", year, code_type))
  raw <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
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
