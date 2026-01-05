#' @description  Color assignment helper for polygon groups
#' assign_group_colors(sf_obj, group_col, colors = RColorBrewer::brewer.pal(5, "Set2"), fixed = NULL, prev_colors = NULL)
#' - sf_obj: an sf object containing the grouping column
#' - group_col: name of the grouping column as a string (e.g., "cluster" or "UEA")
#' - colors: character vector of color hex codes
#' - fixed: optional list(value = <group value to pin>, color = <hex string>)
#' - prev_colors: optional mapping signature -> color (named vector or data.frame with columns signature,color)
assign_group_colors <- function(sf_obj, group_col, colors = RColorBrewer::brewer.pal(5, "Set2"), fixed = NULL, prev_colors = NULL){
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
  if (!"JISCODE" %in% names(sf_obj)){
    warning("sf_obj does not contain 'JISCODE' column; prev_colors matching by membership will be disabled")
    signatures <- rep(NA_character_, length(unique(grp[[group_col]])))
    group_vals <- unique(grp[[group_col]])
  } else {
    group_vals <- unique(grp[[group_col]])
    signatures <- vapply(group_vals, function(g){
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
  if (!is.null(prev_colors) && !is.null(grp_signature)){
    # normalize prev_colors to named character vector: names = signature, values = color
    if (is.data.frame(prev_colors)){
      if (!all(c("signature","color") %in% names(prev_colors))) stop("prev_colors data.frame must contain columns 'signature' and 'color'")
      prev_map <- setNames(as.character(prev_colors$color), as.character(prev_colors$signature))
    } else if (is.character(prev_colors) && !is.null(names(prev_colors))){
      prev_map <- prev_colors
    } else {
      stop("prev_colors must be a named character vector or a data.frame/tibble with columns 'signature' and 'color'")
    }
    matched <- which(grp_signature %in% names(prev_map))
    if (length(matched) > 0){
      color_assignment[matched] <- prev_map[grp_signature[matched]]
    }
  }

  # apply fixed seed if provided (explicit fixed overrides prev_colors)
  if (!is.null(fixed) && !is.null(fixed$value) && !is.null(fixed$color)){
    idx <- which(grp[[group_col]] == fixed$value)
    if (length(idx) > 0) color_assignment[idx] <- fixed$color
  }

  # assign colors using Welsh–Powell greedy ordering (degree descending)
  deg <- vapply(neighbors, length, integer(1))
  order_idx <- order(deg, decreasing = TRUE)

  palette <- colors
  # ensure fixed color is present in the palette
  if (!is.null(fixed) && !is.null(fixed$color) && !(fixed$color %in% palette)){
    palette <- c(palette, fixed$color)
  }

  for (v in order_idx){
    if (!is.na(color_assignment[v])) next
    nbcols <- color_assignment[neighbors[[v]]]
    nbcols <- nbcols[!is.na(nbcols)]
    available <- setdiff(palette, nbcols)
    # if no available color, extend palette and retry
    while (length(available) == 0){
      new_n <- max(4, length(palette) * 2)
      palette <- c(palette, grDevices::colorRampPalette(palette)(new_n)[(length(palette)+1):new_n])
      available <- setdiff(palette, nbcols)
    }
    color_assignment[v] <- available[1]
  }

  # verification: ensure no adjacent groups share the same color
  conflicts <- character(0)
  for (i in seq_along(neighbors)){
    for (j in neighbors[[i]]){
      if (i < j && !is.na(color_assignment[i]) && !is.na(color_assignment[j]) && color_assignment[i] == color_assignment[j]){
        conflicts <- c(conflicts, paste0(grp[[group_col]][i], "-", grp[[group_col]][j], ":", color_assignment[i]))
      }
    }
  }
  if (length(conflicts) > 0){
    stop("Coloring conflict detected between adjacent groups: ", paste(conflicts, collapse = "; "))
  }

  out <- data.frame(grp[[group_col]], color = color_assignment, signature = grp_signature, stringsAsFactors = FALSE)
  names(out)[1] <- group_col
  out <- dplyr::as_tibble(out)
  return(out)
}

#' @description Load persisted color map for a given kind (e.g., "CZ", "UEA") from directory
load_color_map <- function(kind, dir = "output/color_map"){
  file <- file.path(dir, paste0(kind, "_signature_color.csv"))
  if (!file.exists(file)) return(NULL)
  m <- readr::read_csv(file, show_col_types = FALSE)
  if (!all(c("signature","color") %in% names(m))) stop("Color map file must contain 'signature' and 'color' columns")
  return(m %>% dplyr::select(signature, color))
}

#' @description Save/merge color map: keep existing entries and append new ones
save_color_map <- function(map_df, kind, dir = "output/color_map"){
  if (is.null(map_df) || nrow(map_df) == 0) return(invisible(NULL))
  if (!all(c("signature","color") %in% names(map_df))) stop("map_df must contain 'signature' and 'color' columns")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(dir, paste0(kind, "_signature_color.csv"))
  if (file.exists(file)){
    existing <- readr::read_csv(file, show_col_types = FALSE)
  } else {
    existing <- tibble::tibble(signature = character(), color = character())
  }
  combined <- dplyr::bind_rows(existing, map_df) %>%
    dplyr::distinct(signature, .keep_all = TRUE)
  readr::write_csv(combined, file)
  return(invisible(combined))
}