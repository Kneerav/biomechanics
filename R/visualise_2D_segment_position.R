#' Visualize 2D Marker Data with Segments
#'
#' This function visualizes marker data in a 2D plot (sagittal, frontal, or transverse plane)
#' and draws lines between markers that belong to the same segment. The plot also supports
#' adding a legend for the segments.
#'
#' @param data A data frame containing the marker data (including columns for X, Y, Z positions).
#' @param selected_frame An integer specifying the frame number to visualize.
#' @param mapping_data A data frame that maps each marker to a segment.
#' @param marker_col The column in `mapping_data` that contains the marker names.
#' @param segment The name of the column in `mapping_data` that indicates the segment to which each marker belongs.
#' @param plane A character string specifying the plane to plot. Options are "sagittal", "frontal", or "transverse". Default is "sagittal".
#' @param point_size A numeric value specifying the size of the points in the plot. Default is 2.
#' @param show_legend A logical indicating whether to show the legend. Default is TRUE.
#'
#' @return A ggplot2 object representing the 2D visualization of the marker data and segments.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import scales
#'
#' @export

visualise_2D_segment_position <- function(data, selected_frame, mapping_data, marker_col, segment, plane = "sagittal", point_size = 2, show_legend = TRUE) {

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
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("The 'scales' package is required. Please install it.")
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

  # Get the unique segments and assign colors
  unique_segments <- unique(mapping_data[[segment]])
  segment_colors <- scales::hue_pal()(length(unique_segments))

  # Define the plot based on the selected plane
  if (plane == "sagittal") {
    p <- ggplot(long_data, aes(x = X, y = Y)) +
      geom_point(size = point_size) +
      labs(title = paste("Sagittal Plane at Frame =", selected_frame), x = "X Position", y = "Y Position") +
      theme_minimal() +
      coord_equal(xlim = c(min_x, max_x), ylim = c(min_y, max_y))  # Adjusted limits for the sagittal plane

  } else if (plane == "frontal") {
    p <- ggplot(long_data, aes(x = Z, y = Y)) +
      geom_point(size = point_size) +
      labs(title = paste("Frontal Plane at Frame =", selected_frame), x = "Z Position", y = "Y Position") +
      theme_minimal() +
      coord_equal(xlim = c(min_z, max_z), ylim = c(min_y, max_y))  # Adjusted limits for the frontal plane

  } else if (plane == "transverse") {
    p <- ggplot(long_data, aes(x = X, y = Z)) +
      geom_point(size = point_size) +
      labs(title = paste("Transverse Plane at Frame =", selected_frame), x = "X Position", y = "Z Position") +
      theme_minimal() +
      coord_equal(xlim = c(min_x, max_x), ylim = c(min_z, max_z))  # Adjusted limits for the transverse plane

  } else {
    stop("Invalid plane selected. Choose 'sagittal', 'frontal', or 'transverse'.")
  }

  # Create an empty data frame to store line segments
  line_segments <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), color = character(0))

  # Loop through each segment
  for (i in 1:length(unique_segments)) {
    # Get the markers for this segment
    segment_name <- unique_segments[i]
    markers_in_segment <- mapping_data[mapping_data[[segment]] == segment_name, marker_col]

    # Filter the data for the markers in this segment
    segment_data <- long_data[long_data$Marker %in% markers_in_segment, ]

    # If there are at least two markers in the segment, calculate lines between them
    if (length(markers_in_segment) > 1) {
      for (j in 1:(nrow(segment_data) - 1)) {
        for (k in (j + 1):nrow(segment_data)) {
          # Get the positions of the two markers to draw a line
          marker1 <- segment_data[j, ]
          marker2 <- segment_data[k, ]

          # Add the line segment data to the new data frame
          if(plane == "sagittal"){
            line_segments <- rbind(line_segments, data.frame(
              x = marker1$X,
              y = marker1$Y,
              xend = marker2$X,
              yend = marker2$Y,
              segment = segment_name,
              color = segment_colors[i]
            ))
          } else if(plane == "frontal"){
            line_segments <- rbind(line_segments, data.frame(
              x = marker1$Z,
              y = marker1$Y,
              xend = marker2$Z,
              yend = marker2$Y,
              segment = segment_name,
              color = segment_colors[i]
            ))
          } else if(plane == "transverse"){
            line_segments <- rbind(line_segments, data.frame(
              x = marker1$X,
              y = marker1$Z,
              xend = marker2$X,
              yend = marker2$Z,
              segment = segment_name,
              color = segment_colors[i]
            ))
          }

        }
      }
    }
  }

  # Add the line segments to the plot using geom_segment
  p <- p + geom_segment(data = line_segments, aes(x = x, y = y, xend = xend, yend = yend, color = segment), size = 0.5, show.legend = show_legend)

  return(p)
}
