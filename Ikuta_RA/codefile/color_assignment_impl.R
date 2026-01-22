#' @description  Color assignment helper for polygon groups
#' assign_group_colors(sf_obj, group_col, colors = RColorBrewer::brewer.pal(5, "Set2"), fixed = NULL, prev_colors = NULL)
#' - sf_obj: an sf object containing the grouping column
#' - group_col: name of the grouping column as a string (e.g., "cluster" or "UEA")
#' - colors: character vector of color hex codes
#' - fixed: optional list(value = <group value to pin>, color = <hex string>)
#' - prev_colors: optional mapping signature -> color (named vector or data.frame with columns signature,color)
assign_group_colors <- function(sf_obj, group_col, colors = RColorBrewer::brewer.pal(5, "Set2"), fixed = NULL, prev_colors = NULL) {
  #' prev_colors: optional mapping of signatures to colors. Accepts named character vector (names = signature, values = color) or data.frame/tibble with columns `signature` and `color`. When provided, groups whose membership signature matches will reuse the color.
  stopifnot(!missing(sf_obj), !missing(group_col))
  # make sure required packages are available
  if (!requireNamespace("spdep", quietly = TRUE)) stop("spdep is required")
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf is required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr is required")

  # temporarily disable s2 for topology operations and restore to previous setting on exit
  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)

  # build group polygons without relying on pipe or .data (avoids linter warnings)
  grp <- sf_obj[!is.na(sf_obj[[group_col]]), , drop = FALSE]

  # compute membership signature (sorted JISCODEs joined) so we can reuse colors across years
  if (!"JISCODE" %in% names(sf_obj)) {
    warning("sf_obj does not contain 'JISCODE' column; prev_colors matching by membership will be disabled")
    signatures <- rep(NA_character_, length(unique(grp[[group_col]])))
    group_vals <- unique(grp[[group_col]])
  } else {
    group_vals <- unique(grp[[group_col]])
    signatures <- vapply(group_vals, function(g) {
      mem <- grp$JISCODE[grp[[group_col]] == g]
      mem <- sort(unique(as.character(mem)))
      paste(mem, collapse = ",")
    }, character(1))
  }

  # summarize geometries per group
  grp <- dplyr::group_by_at(grp, group_col)
  grp <- dplyr::select_at(grp, group_col)
  grp <- dplyr::summarise(grp)
  grp <- sf::st_make_valid(grp)

  # attach signatures to grp rows in same order
  sig_map <- setNames(signatures, as.character(group_vals))
  grp_signature <- vapply(grp[[group_col]], function(g) sig_map[as.character(g)], character(1))

  neighbors <- spdep::poly2nb(grp)
  color_assignment <- rep(NA_character_, length(neighbors))

  # apply prev_colors mapping (by membership signature) if provided
  if (!is.null(prev_colors) && !is.null(grp_signature)) {
    # normalize prev_colors to named character vector: names = signature, values = color
    if (is.data.frame(prev_colors)) {
      if (!all(c("signature", "color") %in% names(prev_colors))) stop("prev_colors data.frame must contain columns 'signature' and 'color'")
      prev_map <- setNames(as.character(prev_colors$color), as.character(prev_colors$signature))
    } else if (is.character(prev_colors) && !is.null(names(prev_colors))) {
      prev_map <- prev_colors
    } else {
      stop("prev_colors must be a named character vector or a data.frame/tibble with columns 'signature' and 'color'")
    }
    matched <- which(grp_signature %in% names(prev_map))
    if (length(matched) > 0) {
      # Check for conflicts with neighbors before assigning prev_colors
      for (idx in matched) {
        proposed_color <- prev_map[grp_signature[idx]]
        neighbor_colors <- color_assignment[neighbors[[idx]]]
        neighbor_colors <- neighbor_colors[!is.na(neighbor_colors)]
        # only assign if no conflict with already-assigned neighbors
        if (!proposed_color %in% neighbor_colors) {
          color_assignment[idx] <- proposed_color
        }
      }
    }
  }

  # apply fixed seed if provided (explicit fixed overrides prev_colors)
  if (!is.null(fixed) && !is.null(fixed$value) && !is.null(fixed$color)) {
    idx <- which(grp[[group_col]] == fixed$value)
    if (length(idx) > 0) color_assignment[idx] <- fixed$color
  }

  # assign colors using Welsh–Powell greedy ordering (degree descending)
  deg <- vapply(neighbors, length, integer(1))
  order_idx <- order(deg, decreasing = TRUE)

  palette <- colors
  fixed_color <- NULL
  # ensure fixed color is present in the palette and reserve it
  if (!is.null(fixed) && !is.null(fixed$color)) {
    fixed_color <- fixed$color
    if (!(fixed_color %in% palette)) {
      palette <- c(palette, fixed_color)
    }
  }

  for (v in order_idx) {
    if (!is.na(color_assignment[v])) next
    nbcols <- color_assignment[neighbors[[v]]]
    nbcols <- nbcols[!is.na(nbcols)]
    # exclude fixed color from available colors for other groups
    available <- setdiff(palette, c(nbcols, fixed_color))
    # if no available color, extend palette and retry
    while (length(available) == 0) {
      new_n <- max(4, length(palette) * 2)
      palette <- c(palette, grDevices::colorRampPalette(palette)(new_n)[(length(palette) + 1):new_n])
      available <- setdiff(palette, c(nbcols, fixed_color))
    }
    color_assignment[v] <- available[1]
  }

  # verification: ensure no adjacent groups share the same color
  conflicts <- character(0)
  for (i in seq_along(neighbors)) {
    for (j in neighbors[[i]]) {
      if (i < j && !is.na(color_assignment[i]) && !is.na(color_assignment[j]) && color_assignment[i] == color_assignment[j]) {
        conflicts <- c(conflicts, paste0(grp[[group_col]][i], "-", grp[[group_col]][j], ":", color_assignment[i]))
      }
    }
  }
  if (length(conflicts) > 0) {
    stop("Coloring conflict detected between adjacent groups: ", paste(conflicts, collapse = "; "))
  }

  out <- data.frame(grp[[group_col]], color = color_assignment, signature = grp_signature, stringsAsFactors = FALSE)
  names(out)[1] <- group_col
  out <- dplyr::as_tibble(out)
  return(out)
}

#' @description Load persisted color map for a given kind (e.g., "CZ", "UEA") from directory
load_color_map <- function(kind, dir = "output/color_map") {
  file <- file.path(dir, paste0(kind, "_signature_color.csv"))
  if (!file.exists(file)) {
    return(NULL)
  }
  # read as character to avoid type inference issues
  m <- readr::read_csv(file, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
  if (!all(c("signature", "color") %in% names(m))) stop("Color map file must contain 'signature' and 'color' columns")
  m <- m %>% dplyr::mutate(signature = as.character(signature), color = as.character(color))
  return(m %>% dplyr::select(signature, color))
}

#' @description Save/merge color map: keep existing entries and append new ones
save_color_map <- function(map_df, kind, dir = "output/color_map") {
  if (is.null(map_df) || nrow(map_df) == 0) {
    return(invisible(NULL))
  }
  if (!all(c("signature", "color") %in% names(map_df))) stop("map_df must contain 'signature' and 'color' columns")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(dir, paste0(kind, "_signature_color.csv"))
  # ensure types are character
  map_df <- map_df %>% dplyr::mutate(signature = as.character(signature), color = as.character(color))
  if (file.exists(file)) {
    existing <- readr::read_csv(file, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
    existing <- existing %>% dplyr::mutate(signature = as.character(signature), color = as.character(color))
  } else {
    existing <- tibble::tibble(signature = character(), color = character())
  }
  combined <- dplyr::bind_rows(existing, map_df) %>%
    dplyr::distinct(signature, .keep_all = TRUE)
  readr::write_csv(combined, file)
  return(invisible(combined))
}


#' @description to save space, move Hokkaido to north west of the image.
#' @param sf_obj sf object including municipalities in Hokkaido
#' @param jis_col the name containing JISCODE
#' @param shift Numeric vec to adjust position. Default:c(10,4)
#' @return sf object with Hokkaido moved
move_Hokaido <- function(sf_obj, jis_col = "JISCODE", shift = c(10, 4)) {
  #### moving Hokkaido ####
  require(dplyr)
  require(magrittr)
  # To generate enlarge map, we move Hokkaido to upper side by edit geometry.
  movement_Hokkaido <- sf_obj %>%
    dplyr::filter(.data[[jis_col]] %in% (1000:1999)) %>%
    # Minus 10 from longitude and 4 from latitude.
    sf::st_set_geometry(sf::st_geometry(sf_obj %>% dplyr::filter(.data[[jis_col]] %in% (1000:1999))) - shift) %>%
    sf::st_set_crs(4612)
  master <- sf_obj %>%
    dplyr::filter(!(.data[[jis_col]] %in% (1000:1999))) %>%
    dplyr::bind_rows(movement_Hokkaido)
  return(master)
}

#' @description making choropleth maps of CZ or UEA.
#' @param sf_obj sf object with color columun
#' @param lim_x limitation of x coordinate
#' @param lim_y limitation of y coordinate
#' @param linewidth width of lines of munis
#' @param caption caption text
#' @param HokkaidoLine logical, whether to draw Hokkaido separation line
#' @param pref_boundary numeric, width of prefectural boundary lines; 0 to disable
#' @param color_col name of the color column in sf_obj
#' @param caption_size size of caption text
#' @return ggplot2 object
make_basic_plot <- function(
    sf_obj,
    lim_x,
    lim_y,
    caption = NULL,
    HokkaidoLine = FALSE,
    pref_boundary = 0,
    linewidth = .1,
    color_col = "color",
    caption_size = 10) {
    sf_obj %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[color_col]]), linewidth = linewidth) +
      (if (HokkaidoLine) {gen_Hokkaidoline()} else NULL) +
      (if (pref_boundary != 0) {gen_prefectual_boundary(sf_obj, linewidth = pref_boundary)} else NULL) +
      ggplot2::scale_fill_identity() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        plot.caption    = ggplot2::element_text(size = caption_size)
      ) +
      ggplot2::coord_sf(
        ylim = lim_y,
        xlim = lim_x,
        datum = NA
      ) +
      ggplot2::labs(caption = caption) -> out
    return(out)
}

#' @description genarate sprit Line between Hokkaido and others
#' @return ggplot2 layer of Hokkaido separation line
gen_Hokkaidoline <- function() {
  HokkaidoLine <- rbind(c(137.5, 45), c(137.5, 40), c(134, 37), c(120, 37)) %>%
    sf::st_linestring() %>%
    sf::st_sfc(crs = 4612) %>%
    sf::st_sf()
  return(ggplot2::geom_sf(data = HokkaidoLine, linewidth = .1))
}

#' @description generate prefectural boundary lines
#' @param muni_sf sf object of municipalities
#' @param linewidth width of lines of prefectural boundaries
#' @param linecolor color of the lines
#' @return ggplot2 layer of prefectural boundary lines
gen_prefectual_boundary <- function(muni_sf, linewidth = .3, linecolor = "black") {
  if (!"JISCODE" %in% names(muni_sf) || !is.numeric(muni_sf$JISCODE)) {
    stop("muni_sf must contain 'JISCODE' column of numeric type")
  }
  pref_sf <- muni_sf %>%
    sf::st_make_valid() %>% 
    # sf::st_transform(crs = 6677) %>%
    dplyr::mutate(JISCODE = trunc(.data$JISCODE / 1000)) %>%
    # sf::st_buffer(dist = 1000) %>%
    dplyr::group_by(.data$JISCODE) %>%
    dplyr::summarise()  %>% 
    sf::st_cast("MULTILINESTRING") 
    # sf::st_transform(crs = sf::st_crs(muni_sf))
  return(ggplot2::geom_sf(data = pref_sf, color = linecolor, linewidth = linewidth))
}


