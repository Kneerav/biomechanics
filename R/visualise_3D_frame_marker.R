#' Visualize 3D Marker Frame Position
#'
#' This function visualizes the 3D positions of markers at a specified time point
#' from a dataset. It can also optionally display a reference frame.
#'
#' @param data A data frame containing marker data, including columns with '_X', '_Y', and '_Z' suffixes for marker positions.
#' @param selected_time A numeric value representing the time point at which to visualize the markers.
#' @param reference_frame A 3x3 matrix representing a reference frame (optional). Each column should represent an axis of the reference frame.
#' @param origin A 1x3 numeric vector representing the origin point of the reference frame (optional).
#'
#' @return A Plotly 3D scatter plot visualizing the marker positions at the selected time point, with optional reference frame axes.
#'
#' @import plotly
#' @export
visualize_3D_marker_frame_position <- function(data, selected_time, reference_frame, origin) {

  # Check if the selected time exists in the dataframe
  if (!(selected_time %in% data$Time)) {
    stop("Selected time point not found in the dataframe.")
  }

  require(plotly)

  # Filter data for the selected time point
  data_at_time <- data[data$Time == selected_time, ]

  # Extract marker columns
  marker_cols <- grep("_X$|_Y$|_Z$", colnames(data), value = TRUE)
  marker_data <- data_at_time[, marker_cols]

  # Reshape data for plotting
  markers <- unique(gsub("_X|_Y|_Z", "", marker_cols))
  plot_data <- data.frame(
    Marker = rep(markers, each = 3),
    Axis = rep(c("X", "Y", "Z"), times = length(markers)),
    Value = as.numeric(unlist(marker_data))
  )

  # Compute min and max values for X, Y, Z
  axis_min <- min(plot_data$Value)
  axis_max <- max(plot_data$Value)

  # Create the plot
  p <- plot_ly(data = plot_data,
               x = ~Value[plot_data$Axis == "X"],
               y = ~Value[plot_data$Axis == "Z"],
               z = ~Value[plot_data$Axis == "Y"],
               type = "scatter3d",
               mode = "markers",
               #text = ~Marker[plot_data$Axis == "X"],
               marker = list(size = 3, color = "dodgerblue4"),
               hoverinfo = 'text+x+z+y') %>%
    layout(scene = list(
      xaxis = list(title = 'Anterior-Posterior', range = c(axis_min, axis_max)),
      yaxis = list(title = 'Mediolateral', range = c(axis_max, axis_min)),
      zaxis = list(title = 'Vertical', range = c(axis_min, axis_max)),
      aspectmode = 'cube'
    ),
    title = paste("3D Marker Positions at Time =", selected_time))

  # Add reference frame if provided
  if (!is.null(reference_frame)) {
    # Assuming reference_frame is a 3x3 matrix (each column is a direction vector)
    # and origin is a 1x3 vector (the origin point)

    # Extract the axes directions from the reference frame matrix
    e1 <- reference_frame[, 1]
    e2 <- reference_frame[, 2]
    e3 <- reference_frame[, 3]

    # try to normalise, not working
    e1 <- reference_frame[, 1] / sqrt(sum(reference_frame[, 1]^2))
    e2 <- reference_frame[, 2] / sqrt(sum(reference_frame[, 2]^2))
    e3 <- reference_frame[, 3] / sqrt(sum(reference_frame[, 3]^2))

    scale_factor <- 0.2 * (axis_max - axis_min)

    # Add lines representing the reference frame axes
    p <- p %>%
      add_trace(
        x = c(origin[1], origin[1] + scale_factor * e1[1]),
        y = c(origin[3], origin[3] + scale_factor * e1[3]),
        z = c(origin[2], origin[2] + scale_factor * e1[2]),
        type = 'scatter3d',
        mode = 'lines',
        line = list(color = 'red', width = 4),
        name = 'X-axis'
      ) %>%
      add_trace(
        x = c(origin[1], origin[1] + scale_factor *e2[1]),
        y = c(origin[3], origin[3] + scale_factor *e2[3]),
        z = c(origin[2], origin[2] + scale_factor *e2[2]),
        type = 'scatter3d',
        mode = 'lines',
        line = list(color = 'green', width = 4),
        name = 'Y-axis'
      ) %>%
      add_trace(
        x = c(origin[1], origin[1] + scale_factor *e3[1]),
        y = c(origin[3], origin[3] + scale_factor *e3[3]),
        z = c(origin[2], origin[2] + scale_factor *e3[2]),
        type = 'scatter3d',
        mode = 'lines',
        line = list(color = 'blue', width = 4),
        name = 'Z-axis'
      )
  }

  return(p)
}
