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
