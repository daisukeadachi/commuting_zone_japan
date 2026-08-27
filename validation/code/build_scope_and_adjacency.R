# Geographic scope and the municipality adjacency graph.
#
# A municipality is in scope if it sits on one of the four main islands or on the
# Okinawa main island, or is joined to one of them by a permanent road link. The
# rule is implemented as the connected components of queen contiguity on the 2015
# boundary layer, followed by an explicit list of fixed links.
#
# Writes validation/data/municipality_scope.csv and validation/data/adjacency_edges.csv.
# Both are inputs to the Fowler (2024) replication (issue #11) and to the
# contiguity-constrained clustering (issue #12).

suppressMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(igraph)
})
source("validation/code/config.R")
sf_use_s2(FALSE)

# Permanent road links between municipalities whose polygons do not touch.
# Within a block these attach a bridge-connected island to its main island; across
# blocks they carry the four crossings over which people commute daily. The Seikan
# tunnel is absent because it carries rail only.
road_links <- tribble(
  ~code_i,  ~code_j,  ~link,                  ~kind,
  "28100",  "28226",  "Akashi-Kaikyo Bridge", "between blocks",
  "28224",  "36202",  "Onaruto Bridge",       "between blocks",
  "33202",  "37203",  "Seto Ohashi",          "between blocks",
  "34205",  "38202",  "Shimanami Kaido",      "between blocks",
  "35201",  "40100",  "Kanmon crossing",      "between blocks",
  "34202",  "34215",  "Kurahashi bridges",    "within block",
  "35212",  "35305",  "Oshima Bridge",        "within block",
  "43213",  "43212",  "Amakusa bridges",      "within block",
  "46206",  "46404",  "Kuronoseto Bridge",    "within block"
)

# Islands reachable only by ferry whose polygon nonetheless shares a boundary with a
# main-island municipality, drawn over water in the boundary layer.
water_boundary_artifacts <- c("37364")

message("reading the boundary layer")
shape <- st_read(boundary_shp, quiet = TRUE, options = "ENCODING=CP932") %>%
  st_make_valid() %>%
  mutate(code = merge_tokyo_wards(sprintf("%05d", as.integer(as.character(JISCODE)))))

# Dissolve the special wards into the single unit the rest of the pipeline expects.
shape <- shape %>%
  group_by(code) %>%
  summarise(NAME = ifelse(first(code) == tokyo_merged_code, tokyo_merged_name, first(NAME)),
            PNAME = first(PNAME), .groups = "drop") %>%
  st_make_valid()

# Queen contiguity. Two municipalities are neighbours when their polygons share any
# boundary point.
touching <- st_intersects(shape, shape)
contiguity <- do.call(rbind, lapply(seq_along(touching), function(i) {
  j <- setdiff(touching[[i]], i)
  if (length(j)) cbind(i, j) else NULL
}))
contiguity <- tibble(code_i = shape$code[contiguity[, 1]], code_j = shape$code[contiguity[, 2]]) %>%
  filter(code_i < code_j) %>%
  distinct()

# Remove the edges that exist only because a municipal boundary is drawn across water.
contiguity <- contiguity %>%
  filter(!(code_i %in% water_boundary_artifacts | code_j %in% water_boundary_artifacts))

# Blocks are the connected components of contiguity alone, before any road link is
# added, so that each block is one landmass.
land_graph <- graph_from_data_frame(contiguity, directed = FALSE, vertices = tibble(name = shape$code))
land_component <- components(land_graph)$membership[shape$code]
block_sizes <- sort(table(land_component), decreasing = TRUE)
main_components <- as.integer(names(block_sizes)[1:5])

# Name each block by the prefecture that holds most of it.
block_label <- c("Honshu", "Kyushu", "Hokkaido", "Shikoku", "Okinawa main island")
names(block_label) <- as.character(main_components)

scope <- shape %>%
  st_drop_geometry() %>%
  transmute(
    code,
    muni_name = NAME,
    prefecture = PNAME,
    land_component = as.integer(land_component)
  )

# A municipality is in scope when a permanent road link, or an unbroken chain of them,
# joins its landmass to one of the five main blocks. Taking the components of the
# road-augmented graph propagates the link to the whole island, so that Amakusa city
# and Reihoku enter with Kami-Amakusa and the three Awaji municipalities enter together.
road_edges <- road_links %>% transmute(from = code_i, to = code_j)
augmented <- graph_from_data_frame(
  bind_rows(contiguity %>% transmute(from = code_i, to = code_j), road_edges),
  directed = FALSE, vertices = tibble(name = shape$code)
)
augmented_component <- components(augmented)$membership[shape$code]
in_scope_components <- unique(augmented_component[scope$land_component %in% main_components])
scope$in_scope <- augmented_component %in% in_scope_components

# Each block is named for its landmass. An island attached by road takes the block of
# the main island at the far end of its link; where an island is attached to two blocks,
# as Awaji is to Honshu and to Shikoku, the first link listed in road_links decides.
scope$block <- unname(block_label[as.character(scope$land_component)])
for (r in seq_len(nrow(road_links))) {
  ends <- c(road_links$code_i[r], road_links$code_j[r])
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
    code %in% water_boundary_artifacts ~ "ferry-only island",
    TRUE ~ "island with no permanent road link to a main island"
  ))

# Area on the equal-area projection, the counterpart of Fowler's EPSG:5070.
area_km2 <- shape %>%
  st_transform(crs_equal_area) %>%
  st_area() %>%
  as.numeric() / 1e6
scope$area_km2 <- round(area_km2[match(scope$code, shape$code)], 3)

# The adjacency graph the constrained clustering runs on: contiguity plus every
# permanent road link, for every municipality rather than for the scope alone. Two
# municipalities on the same offshore island share a boundary, so the components of this
# graph are the road-connected landmasses of the whole country, and a delineation that
# covers the offshore islands needs no adjacency written by hand.
edges <- bind_rows(
  contiguity %>% mutate(link = "shared boundary", kind = "contiguity"),
  road_links %>%
    mutate(lo = pmin(code_i, code_j), hi = pmax(code_i, code_j)) %>%
    transmute(code_i = lo, code_j = hi, link, kind = "road link")
) %>%
  distinct(code_i, code_j, .keep_all = TRUE) %>%
  arrange(code_i, code_j)

full_graph <- graph_from_data_frame(edges, directed = FALSE,
                                    vertices = tibble(name = sort(scope$code)))
scope$graph_component <- as.integer(components(full_graph)$membership[scope$code])

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(scope, file.path(data_dir, "municipality_scope.csv"))
write_csv(edges, file.path(data_dir, "adjacency_edges.csv"))

message(sprintf("blocks: %s", paste(sprintf("%s %d", block_label[as.character(main_components)],
                                            as.integer(block_sizes[1:5])), collapse = ", ")))
message(sprintf("in scope: %d municipalities; excluded: %d", sum(scope$in_scope), sum(!scope$in_scope)))
message(sprintf("adjacency edges: %d contiguity, %d road links",
                sum(edges$kind == "contiguity"), sum(edges$kind == "road link")))
message(sprintf("components of the adjacency graph: %d (sizes %s)",
                components(full_graph)$no,
                paste(sort(components(full_graph)$csize, decreasing = TRUE), collapse = ", ")))
message(sprintf("components holding no municipality in scope: %d over %d municipalities",
                n_distinct(scope$graph_component[!scope$in_scope]), sum(!scope$in_scope)))
