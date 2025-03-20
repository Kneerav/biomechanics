#' Visualize 3D Marker Position
#'
#' Visualizes the 3D positions of markers at a specific time point using interactive 3D plots.
#'
#' @param data A dataframe containing time and marker positions with columns formatted as
#'              `Time`, `markername_X`, `markername_Y`, `markername_Z`.
#' @param selected_time Numeric value indicating the specific time point at which to visualize the marker positions.
#'
#' @return A `plotly` object representing the 3D scatter plot of marker positions.
#'
#' @import plotly
#' @export
visualise_3D_marker_position <- function(data, selected_time) {

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

  # could update so text is optional? "markers+text"
  # could update for option to highlight markers?
  # could update to add reference frames?
  # could update to remove gridlines

  # Plotting using plotly
  p <- plot_ly(data = plot_data,
               x = ~Value[plot_data$Axis == "X"],
               y = ~Value[plot_data$Axis == "Z"],
               z = ~Value[plot_data$Axis == "Y"],
               #color = ~Marker[plot_data$Axis == "X"],
               type = "scatter3d",
               hoverinfo = 'text+x+z+y',
               mode = "markers",
               text = ~Marker[plot_data$Axis == "X"],
               marker = list(size = 3, color = "dodgerblue4")) %>%
    layout(scene = list(
      xaxis = list(title = 'Anterior-Posterior', range = c(axis_min, axis_max)),
      yaxis = list(title = 'Mediolateral', range = c(axis_max, axis_min)),
      zaxis = list(title = 'Vertical', range = c(axis_min, axis_max)),
      aspectmode = 'cube'
    ),
    title = paste("3D Marker Positions at Time =", selected_time))

  p
}
