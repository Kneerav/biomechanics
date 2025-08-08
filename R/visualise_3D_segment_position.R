#' Visualize 3D Marker Positions and Segments
#'
#' This function visualizes the 3D positions of markers at a specific frame,
#' connecting markers belonging to the same body segment with lines to represent
#' the segments of the body in a 3D space.
#'
#' @param data A data frame containing time and marker positions with columns formatted as
#'              `Frame`, `Time`, `markername_X`, `markername_Y`, `markername_Z`.
#' @param selected_frame Numeric value indicating the frame to visualize the marker positions.
#' @param mapping_data A data frame containing mapping information, with columns for marker names
#'                     and associated body segments.
#' @param marker_col Name of the column in `mapping_data` that contains the marker names.
#' @param segment Name of the column in `mapping_data` that contains the body segments.
#'
#' @return A `plotly` object representing the 3D scatter plot of marker positions with segment lines.
#'
#' @import dplyr
#' @import plotly
#' @import scales
#' @import stringr
#'
#' @export
visualise_3D_segment_position <- function(data, selected_frame, mapping_data, marker_col, segment) {

  # Check if the selected frame exists in the dataframe
  if (selected_frame < 1 || selected_frame > nrow(data)) {
    stop("Selected frame number is out of bounds.")
  }

  # Ensure the marker_col and segment columns are present in the mapping_data
  if (!(marker_col %in% colnames(mapping_data)) | !(segment %in% colnames(mapping_data))) {
    stop("Columns for marker names or segments are missing in mapping_data.")
  }

  # Filter data for the selected frame (row)
  data_at_frame <- data[selected_frame, ]

  # Extract marker columns (those ending with _X, _Y, _Z)
  marker_cols <- grep("_X$|_Y$|_Z$", colnames(data), value = TRUE)

  # Extract the markers from column names (without the _X, _Y, _Z)
  markers <- unique(gsub("_X$|_Y$|_Z$", "", marker_cols))

  # Create a plot for the 3D markers
  plot_data <- data.frame()
  for (marker in markers) {
    # Extract X, Y, Z positions for the marker
    marker_x <- data_at_frame[[paste0(marker, "_X")]]
    marker_y <- data_at_frame[[paste0(marker, "_Y")]]
    marker_z <- data_at_frame[[paste0(marker, "_Z")]]

    # Add marker position data for plotting
    plot_data <- rbind(plot_data, data.frame(
      Marker = marker,
      X = marker_x,
      Y = marker_y,
      Z = marker_z
    ))
  }

  # Calculate axis limits (min and max values for X, Y, Z)
  axis_min <- min(c(plot_data$X, plot_data$Y, plot_data$Z))
  axis_max <- max(c(plot_data$X, plot_data$Y, plot_data$Z))

  # Reshape data for plotting (same as your original function)
  plot_data_long <- data.frame(
    Marker = rep(markers, each = 3),
    Axis = rep(c("X", "Y", "Z"), times = length(markers)),
    Value = as.numeric(unlist(data_at_frame[, marker_cols]))
  )

  # Create the base 3D scatter plot of the markers
  p <- plot_ly(data = plot_data_long,
               x = ~Value[plot_data_long$Axis == "X"],
               y = ~Value[plot_data_long$Axis == "Z"],
               z = ~Value[plot_data_long$Axis == "Y"],
               type = "scatter3d",
               hoverinfo = 'text+x+z+y',
               mode = "markers",
               #text = ~Marker[plot_data_long$Axis == "X"],
               marker = list(size = 3, color = "#024950"),
               showlegend=FALSE)

  # Define a vector of unique colors for each segment (you can customize this list)
  segment_colors <- scales::hue_pal()(length(unique(mapping_data[[segment]])))

  # Loop through the segments and add lines connecting the markers
  for (body_segment in unique(mapping_data[[segment]])) {

    # Get the markers belonging to this body segment
    markers_in_segment <- mapping_data[mapping_data[[segment]] == body_segment, marker_col]

    # Filter out markers not present in the current data
    markers_in_segment <- markers_in_segment[markers_in_segment %in% plot_data$Marker]

    if (length(markers_in_segment) < 2) {
      next # Skip segments with fewer than 2 markers (no segments to connect)
    }

    # Get the positions of these markers
    segment_positions <- plot_data[plot_data$Marker %in% markers_in_segment, ]

    # Get a color for this segment from the predefined set of colors
    segment_color <- segment_colors[which(unique(mapping_data[[segment]]) == body_segment)]

    # Add lines between all pairs of markers within the segment
    for (i in 1:(nrow(segment_positions) - 1)) {
      for (j in (i + 1):nrow(segment_positions)) {

        # Create a line connecting marker i and marker j (respecting axis order)
        p <- p %>%
          add_trace(
            x = c(segment_positions$X[i], segment_positions$X[j]),
            y = c(segment_positions$Z[i], segment_positions$Z[j]),  # Corrected axis order: Z as mediolateral
            z = c(segment_positions$Y[i], segment_positions$Y[j]),  # Corrected axis order: Y as vertical
            type = 'scatter3d',
            mode = 'lines',
            text=NA,
            line = list(color = segment_color, width = 3),  # Use unique color for each segment
            name = paste(body_segment, "Segment")
          )
      }
    }
  }

  # Adjust the layout with axis limits and aspect ratio to match the original function
  p <- p %>%
    layout(scene = list(
      xaxis = list(title = 'Anterior-Posterior', range = c(axis_min, axis_max)),
      yaxis = list(title = 'Mediolateral', range = c(axis_max, axis_min)),  # Y-axis as Mediolateral
      zaxis = list(title = 'Vertical', range = c(axis_min, axis_max)),  # Z-axis as Vertical
      aspectmode = 'cube',  # Ensures that the plot is proportionate
      showlegend = FALSE  # Hide the legend
    ),
    title = paste("3D Marker Positions at Frame =", selected_frame))

  # Return the plot
  return(p)
}
