# Geographic scope and the municipality adjacency graph, on each census date's own codes.
#
# build_scope_and_adjacency.R does this once, on the units in force on 1 October 2015. A
# delineation read on each year's own municipality codes needs the same two tables per
# census date, because the units themselves differ: 3,256 municipalities in 1980 against
# 1,714 in 2015, and the mergers that closed the gap redraw both the scope and the
# adjacency graph.
#
# The rule is the one the 2015 tables follow. A municipality is in scope if it sits on one
# of the four main islands or on the Okinawa main island, or is joined to one of them by a
# permanent road link. Blocks are the connected components of queen contiguity alone.
#
# The road links and the water-boundary artifact are declared in config.R on the 2015
# codes, so each end is carried back onto the year's codes through the Municipality Map
# Maker crosswalk. Where a 2015 unit was several municipalities at the earlier date, the
# end of the link is the one whose polygon lies closest to the other end.
#
# The boundary layers come from fetch_boundaries.py. The 2020 census is read on the 2015
# layer, since no boundary moved after 2015 and only two towns took new codes.
#
# Writes validation/data/original/municipality_scope_<year>.csv and
# validation/data/original/adjacency_edges_<year>.csv.

suppressMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(igraph)
})
Sys.setenv(CZ_CODES = "original")
source("validation/code/config.R")
sf_use_s2(FALSE)

stopifnot(code_universe == "original")
out_dir <- file.path(data_dir, "original")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

crosswalk_dir <- file.path(data_dir, "crosswalk")

#' The municipalities of one census date that fall inside a 2015 unit.
#'
#' Read off the crosswalk that maps the date's codes onto the 2015 codes. 2015 is the
#' identity, and 2020 is the identity once the two incorporated towns are renamed, which
#' rename_incorporated_towns does.
#'
#' @param year census year
#' @return named character vector of the year's codes, keyed by the 2015 code
predecessors <- function(year) {
  if (year >= 2015L) return(NULL)
  x <- suppressWarnings(read_csv(
    file.path(crosswalk_dir, sprintf("codelist_%d1001and20151001.csv", year)),
    locale = locale(encoding = "CP932"), col_types = cols(.default = col_character())))
  setNames(merge_tokyo_wards(x$JISCODE1), merge_tokyo_wards(x$JISCODE2))
}

#' Carry a list of 2015 codes onto one census date's codes.
#'
#' @param codes_2015 character vector of 2015 municipality codes
#' @param map the year's predecessor table, or NULL when the date is 2015 or later
#' @param present the year's municipality codes, so that a predecessor absent from the
#'   layer is dropped
carry_back <- function(codes_2015, map, present) {
  out <- lapply(codes_2015, function(code) {
    candidates <- if (is.null(map)) code else unname(map[names(map) == code])
    intersect(candidates, present)
  })
  names(out) <- codes_2015
  out
}

build_year <- function(year) {
  message("scope and adjacency for ", year)
  shape <- st_read(boundary_path(year), quiet = TRUE, options = "ENCODING=CP932") %>%
    st_make_valid() %>%
    mutate(code = merge_tokyo_wards(
      rename_incorporated_towns(sprintf("%05d", as.integer(as.character(JISCODE))), year)))

  # Dissolve the special wards into the single unit the rest of the pipeline expects.
  shape <- shape %>%
    group_by(code) %>%
    summarise(NAME = ifelse(first(code) == tokyo_merged_code, tokyo_merged_name, first(NAME)),
              PNAME = first(PNAME), .groups = "drop") %>%
    st_make_valid()

  touching <- st_intersects(shape, shape)
  contiguity <- do.call(rbind, lapply(seq_along(touching), function(i) {
    j <- setdiff(touching[[i]], i)
    if (length(j)) cbind(i, j) else NULL
  }))
  contiguity <- tibble(code_i = shape$code[contiguity[, 1]],
                       code_j = shape$code[contiguity[, 2]]) %>%
    filter(code_i < code_j) %>%
    distinct()

  # Distances are measured on the equal-area projection, as areas are.
  projected <- st_transform(shape, crs_equal_area)

  map <- predecessors(year)
  ferry_only <- unlist(carry_back(water_boundary_artifacts, map, shape$code), use.names = FALSE)
  contiguity <- contiguity %>%
    filter(!(code_i %in% ferry_only | code_j %in% ferry_only))

  # Each end of a road link is the municipality of this census date, inside the 2015 unit
  # the link names, whose polygon lies closest to the other end. Where the 2015 unit was
  # one municipality already this is that municipality.
  ends_i <- carry_back(road_links$code_i, map, shape$code)
  ends_j <- carry_back(road_links$code_j, map, shape$code)
  year_links <- bind_rows(lapply(seq_len(nrow(road_links)), function(r) {
    a <- ends_i[[r]]; b <- ends_j[[r]]
    if (!length(a) || !length(b)) {
      stop(sprintf("%s has no end in %d", road_links$link[r], year))
    }
    gap <- st_distance(projected[match(a, shape$code), ], projected[match(b, shape$code), ])
    nearest <- which(gap == min(gap), arr.ind = TRUE)[1, ]
    tibble(code_i = a[nearest[1]], code_j = b[nearest[2]],
           link = road_links$link[r], kind = road_links$kind[r])
  }))

  land_graph <- graph_from_data_frame(contiguity, directed = FALSE,
                                      vertices = tibble(name = shape$code))
  land_component <- components(land_graph)$membership[shape$code]
  block_sizes <- sort(table(land_component), decreasing = TRUE)
  main_components <- as.integer(names(block_sizes)[1:5])
  block_label <- c("Honshu", "Kyushu", "Hokkaido", "Shikoku", "Okinawa main island")
  names(block_label) <- as.character(main_components)

  scope <- shape %>%
    st_drop_geometry() %>%
    transmute(code, muni_name = NAME, prefecture = PNAME,
              land_component = as.integer(land_component))

  augmented <- graph_from_data_frame(
    bind_rows(contiguity %>% transmute(from = code_i, to = code_j),
              year_links %>% transmute(from = code_i, to = code_j)),
    directed = FALSE, vertices = tibble(name = shape$code))
  augmented_component <- components(augmented)$membership[shape$code]
  in_scope_components <- unique(augmented_component[scope$land_component %in% main_components])
  scope$in_scope <- augmented_component %in% in_scope_components

  scope$block <- unname(block_label[as.character(scope$land_component)])
  for (r in seq_len(nrow(year_links))) {
    ends <- c(year_links$code_i[r], year_links$code_j[r])
    comps <- scope$land_component[match(ends, scope$code)]
    main_side <- which(comps %in% main_components)
    if (length(main_side) != 1) next
    attached <- comps[setdiff(1:2, main_side)]
    fill <- is.na(scope$block) & scope$land_component == attached
    scope$block[fill] <- block_label[as.character(comps[main_side])]
  }

  scope <- scope %>%
    mutate(exclusion_reason = case_when(
      in_scope ~ NA_character_,
      code %in% c("01695", "01696", "01698") ~ "Northern Territories, no census",
      code %in% ferry_only ~ "ferry-only island",
      TRUE ~ "island with no permanent road link to a main island"))

  area_km2 <- as.numeric(st_area(projected)) / 1e6
  scope$area_km2 <- round(area_km2[match(scope$code, shape$code)], 3)

  edges <- bind_rows(
    contiguity %>% mutate(link = "shared boundary", kind = "contiguity"),
    year_links %>%
      mutate(lo = pmin(code_i, code_j), hi = pmax(code_i, code_j)) %>%
      transmute(code_i = lo, code_j = hi, link, kind)) %>%
    distinct(code_i, code_j, .keep_all = TRUE) %>%
    arrange(code_i, code_j)

  full_graph <- graph_from_data_frame(edges, directed = FALSE,
                                      vertices = tibble(name = sort(scope$code)))
  scope$graph_component <- as.integer(components(full_graph)$membership[scope$code])

  write_csv(scope, scope_path(year))
  write_csv(edges, adjacency_path(year))
  tibble(year = year, municipalities = nrow(scope), in_scope = sum(scope$in_scope),
         contiguity_edges = sum(edges$kind == "contiguity"),
         road_links = nrow(year_links), components = components(full_graph)$no)
}

summary_table <- bind_rows(lapply(census_years, build_year))
print(as.data.frame(summary_table))
