#' Visualize 2D Marker Data in Selected Plane
#'
#' This function visualizes marker data in a 2D plot (sagittal, frontal, or transverse plane)
#' at a selected frame. It plots the markers as points on the chosen plane and adjusts the axis limits
#' according to the marker data.
#'
#' @param data A data frame containing the marker data (including columns for X, Y, Z positions).
#' @param selected_frame An integer specifying the frame number to visualize.
#' @param plane A character string specifying the plane to plot. Options are "sagittal", "frontal", or "transverse".
#' @param point_size A numeric value specifying the size of the points in the plot. Default is 2.
#'
#' @return A ggplot2 object representing the 2D visualization of the marker data.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
#' @export
visualise_2D_marker_position <- function(data, selected_frame, plane, point_size = 2) {

  # Ensure required libraries are available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required. Please install it.")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("The 'tidyr' package is required. Please install it.")
  }

  # Get the marker columns (those that end in _X, _Y, _Z)
  marker_cols <- grep("_X$|_Y$|_Z$", colnames(data), value = TRUE)

  # Extract marker names (by removing _X, _Y, _Z)
  markers <- unique(gsub("_X$|_Y$|_Z$", "", marker_cols))

  # Get the full range of marker data across all frames (min/max for X, Y, Z)
  min_x <- min(data[grep("_X$", colnames(data))], na.rm = TRUE)
  max_x <- max(data[grep("_X$", colnames(data))], na.rm = TRUE)
  min_y <- min(data[grep("_Y$", colnames(data))], na.rm = TRUE)
  max_y <- max(data[grep("_Y$", colnames(data))], na.rm = TRUE)
  min_z <- min(data[grep("_Z$", colnames(data))], na.rm = TRUE)
  max_z <- max(data[grep("_Z$", colnames(data))], na.rm = TRUE)

  # Filter the data for the selected frame
  filtered_data <- data %>% dplyr::slice(selected_frame)

  # Reshape the data to a long format for ggplot
  long_data <- filtered_data %>%
    tidyr::pivot_longer(cols = grep("_X$|_Y$|_Z$", colnames(filtered_data)),
                        names_to = c("Marker", ".value"),
                        names_sep = "_")

  # Define the plot based on the selected plane
  if (plane == "sagittal") {
    p <- ggplot(long_data, aes(x = X, y = Y)) +
      geom_point(size = point_size) +
      labs(title = paste("Sagittal Plane at Frame =", selected_frame), x = "X Position", y = "Y Position") +
      theme_minimal() +
      coord_equal(xlim = c(min_x, max_x), ylim = c(min_y, max_y))  # Adjusted limits

  } else if (plane == "frontal") {
    p <- ggplot(long_data, aes(x = Z, y = Y)) +
      geom_point(size = point_size) +
      labs(title = paste("Frontal Plane at Frame =", selected_frame), x = "Z Position", y = "Y Position") +
      theme_minimal() +
      coord_equal(xlim = c(min_z, max_z), ylim = c(min_y, max_y))  # Adjusted limits

  } else if (plane == "transverse") {
    p <- ggplot(long_data, aes(x = X, y = Z)) +
      geom_point(size = point_size) +
      labs(title = paste("Transverse Plane at Frame =", selected_frame), x = "X Position", y = "Z Position") +
      theme_minimal() +
      coord_equal(xlim = c(min_x, max_x), ylim = c(min_z, max_z))  # Adjusted limits

  } else {
    stop("Invalid plane selected. Choose 'sagittal', 'frontal', or 'transverse'.")
  }

  return(p)
}
