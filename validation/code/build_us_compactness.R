# Compactness of United States counties and of the commuting zones built on them.
#
# The Japanese zones score lower on compactness than the source paper's. Part of that can
# come from the method and part from the shape of the units the zones are assembled from,
# and the two are told apart by measuring the building blocks as well as the zones on both
# sides. This computes the United States half; make_diagnostics.R computes the Japanese
# half.
#
# The layer is the one the source paper released, so the county vintage, the zone
# assignment and the projection are all its own rather than reconstructions. Compactness
# is the Polsby-Popper ratio 4 pi A / P^2 averaged over units, which is what the source
# paper's checkCompactness computes; applying it to the undissolved counties gives the
# building-block figure.
#
# Writes validation/output/us_compactness.csv. The archive is downloaded on first use into
# validation/data/us, which is not committed.

suppressMessages({
  library(sf)
  library(dplyr)
  library(readr)
})
source("validation/code/config.R")
sf_use_s2(FALSE)

us_dir <- file.path(data_dir, "us")
archive <- file.path(us_dir, "CommutingZones2020_County_GIS_files.zip")
layer <- file.path(us_dir, "county20.shp")
archive_url <- paste0("https://raw.githubusercontent.com/csfowler/CommutingZones2020/",
                      "main/Output%20Data/CommutingZones2020_County_GIS_files.zip")

# NAD83 / Conus Albers, the equal-area system the source paper projects to before
# measuring area and compactness. The released layer already carries it; naming it here
# keeps the measurement independent of what the file happens to hold.
crs_us_equal_area <- 5070

if (!file.exists(layer)) {
  dir.create(us_dir, showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(archive)) download.file(archive_url, archive, mode = "wb", quiet = TRUE)
  unzip(archive, exdir = us_dir)
}

counties <- st_read(layer, quiet = TRUE) %>%
  st_transform(crs_us_equal_area) %>%
  st_make_valid()

#' Mean Polsby-Popper ratio over the polygons of a layer.
compactness <- function(polygons) {
  perimeter <- as.numeric(st_length(st_cast(st_geometry(polygons), "MULTILINESTRING")))
  mean(4 * pi * as.numeric(st_area(polygons)) / perimeter^2)
}

zones <- counties %>%
  group_by(CZ20) %>%
  summarise(.groups = "drop") %>%
  st_make_valid()

table <- tibble(
  unit = c("county", "commuting zone"),
  year = 2020L,
  units = c(nrow(counties), nrow(zones)),
  compactness = c(compactness(counties), compactness(zones)))

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(table, file.path(output_dir, "us_compactness.csv"))
print(as.data.frame(table))
