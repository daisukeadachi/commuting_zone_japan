# Color assignment helper for polygon groups
# assign_group_colors(sf_obj, group_col, colors = RColorBrewer::brewer.pal(5, "Set2"), fixed = NULL)
# - sf_obj: an sf object containing the grouping column
# - group_col: name of the grouping column as a string (e.g., "cluster" or "UEA")
# - colors: character vector of color hex codes
# - fixed: optional list(value = <group value to pin>, color = <hex string>)

assign_group_colors <- function(sf_obj, group_col, colors = RColorBrewer::brewer.pal(5, "Set2"), fixed = NULL){
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
  grp <- dplyr::group_by_at(grp, group_col)
  grp <- dplyr::select_at(grp, group_col)
  grp <- dplyr::summarise(grp)
  grp <- sf::st_make_valid(grp)

  neighbors <- spdep::poly2nb(grp)
  color_assignment <- rep(NA_character_, length(neighbors))

  # apply fixed seed if provided
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

  # apply fixed assignment if present
  if (!is.null(fixed) && !is.null(fixed$value) && !is.null(fixed$color)){
    fixed_idx <- which(grp[[group_col]] == fixed$value)
    if (length(fixed_idx) > 0) color_assignment[fixed_idx] <- fixed$color
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

  out <- data.frame(grp[[group_col]], color = color_assignment, stringsAsFactors = FALSE)
  names(out)[1] <- group_col
  out <- dplyr::as_tibble(out)
  return(out)
}
