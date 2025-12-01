# R/compare_edits.R

#' Compare Original and Edited Spatial Points
#'
#' Creates a detailed comparison between original and edited sf point objects,
#' showing coordinate changes and distances moved.
#'
#' @param original Original sf POINT object
#' @param edited Edited sf POINT object
#' @param id_column Character. Name of the ID column to match points between datasets
#'
#' @return A data.frame with the following columns:
#' \describe{
#'   \item{ID}{Point identifier}
#'   \item{Orig_Lon}{Original longitude}
#'   \item{Orig_Lat}{Original latitude}
#'   \item{Edit_Lon}{Edited longitude}
#'   \item{Edit_Lat}{Edited latitude}
#'   \item{Delta_Lon}{Change in longitude}
#'   \item{Delta_Lat}{Change in latitude}
#'   \item{Distance_m}{Distance moved in meters}
#'   \item{Modified}{Logical indicating if point was modified}
#' }
#'
#' @details
#' This function is useful for:
#' \itemize{
#'   \item Quality control of coordinate corrections
#'   \item Creating audit trails of spatial edits
#'   \item Identifying which points were moved and by how much
#'   \item Generating reports of coordinate changes
#' }
#'
#' The distance calculation uses the Haversine formula and returns values in meters.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create original points
#' original <- st_sf(
#'   id = 1:3,
#'   geometry = st_sfc(
#'     st_point(c(-122.4194, 37.7749)),
#'     st_point(c(-118.2437, 34.0522)),
#'     st_point(c(-74.0060, 40.7128))
#'   ),
#'   crs = 4326
#' )
#'
#' # Edit points
#' edited <- edit_spatial_points(original, id_column = "id")
#'
#' # Compare
#' comparison <- compare_edits(original, edited, id_column = "id")
#' print(comparison)
#'
#' # View only modified points
#' modified_points <- comparison[comparison$Modified, ]
#' print(modified_points)
#' }
#'
#' @importFrom sf st_coordinates st_crs st_sfc st_point st_distance
#' @export
compare_edits <- function(original, edited, id_column) {

  # Validation
  if (!inherits(original, "sf") || !inherits(edited, "sf")) {
    stop("Both 'original' and 'edited' must be sf objects")
  }

  if (!id_column %in% names(original)) {
    stop(paste0("Column '", id_column, "' not found in original data"))
  }

  if (!id_column %in% names(edited)) {
    stop(paste0("Column '", id_column, "' not found in edited data"))
  }

  if (nrow(original) != nrow(edited)) {
    warning("Original and edited datasets have different numbers of rows")
  }

  # Match points by ID
  common_ids <- intersect(original[[id_column]], edited[[id_column]])

  if (length(common_ids) == 0) {
    stop("No common IDs found between original and edited datasets")
  }

  # Subset to common IDs and order consistently
  original <- original[original[[id_column]] %in% common_ids, ]
  edited <- edited[edited[[id_column]] %in% common_ids, ]

  original <- original[order(original[[id_column]]), ]
  edited <- edited[order(edited[[id_column]]), ]

  # Extract coordinates
  orig_coords <- sf::st_coordinates(original)
  edit_coords <- sf::st_coordinates(edited)

  # Create comparison data frame
  comparison <- data.frame(
    ID = original[[id_column]],
    Orig_Lon = orig_coords[, 1],
    Orig_Lat = orig_coords[, 2],
    Edit_Lon = edit_coords[, 1],
    Edit_Lat = edit_coords[, 2],
    Delta_Lon = edit_coords[, 1] - orig_coords[, 1],
    Delta_Lat = edit_coords[, 2] - orig_coords[, 2],
    stringsAsFactors = FALSE
  )

  # Calculate distance moved for each point
  comparison$Distance_m <- sapply(1:nrow(comparison), function(i) {
    p1 <- sf::st_point(c(comparison$Orig_Lon[i], comparison$Orig_Lat[i]))
    p2 <- sf::st_point(c(comparison$Edit_Lon[i], comparison$Edit_Lat[i]))

    # Use the CRS from the original data
    crs_to_use <- sf::st_crs(original)

    dist <- sf::st_distance(
      sf::st_sfc(p1, crs = crs_to_use),
      sf::st_sfc(p2, crs = crs_to_use)
    )[1]

    as.numeric(dist)
  })

  # Add modified flag
  comparison$Modified <- comparison$Distance_m > 0

  # Add summary statistics as attributes
  attr(comparison, "summary") <- list(
    total_points = nrow(comparison),
    modified_points = sum(comparison$Modified),
    unmodified_points = sum(!comparison$Modified),
    max_distance_m = max(comparison$Distance_m),
    mean_distance_m = mean(comparison$Distance_m[comparison$Modified]),
    median_distance_m = median(comparison$Distance_m[comparison$Modified])
  )

  return(comparison)
}

#' Print Summary of Edit Comparison
#'
#' @param comparison_df Output from compare_edits()
#' @export
print_edit_summary <- function(comparison_df) {

  if (!inherits(comparison_df, "data.frame")) {
    stop("Input must be a data frame from compare_edits()")
  }

  summary_stats <- attr(comparison_df, "summary")

  if (is.null(summary_stats)) {
    # Calculate if not present
    summary_stats <- list(
      total_points = nrow(comparison_df),
      modified_points = sum(comparison_df$Modified),
      unmodified_points = sum(!comparison_df$Modified),
      max_distance_m = max(comparison_df$Distance_m),
      mean_distance_m = mean(comparison_df$Distance_m[comparison_df$Modified]),
      median_distance_m = median(comparison_df$Distance_m[comparison_df$Modified])
    )
  }

  cat("\n===== SPATIAL EDIT SUMMARY =====\n\n")
  cat("Total Points:      ", summary_stats$total_points, "\n")
  cat("Modified Points:   ", summary_stats$modified_points, "\n")
  cat("Unmodified Points: ", summary_stats$unmodified_points, "\n")
  cat("\n")
  cat("Distance Statistics (modified points only):\n")
  cat("  Maximum:  ", round(summary_stats$max_distance_m, 2), "meters\n")
  cat("  Mean:     ", round(summary_stats$mean_distance_m, 2), "meters\n")
  cat("  Median:   ", round(summary_stats$median_distance_m, 2), "meters\n")
  cat("\n================================\n\n")

  invisible(summary_stats)
}

