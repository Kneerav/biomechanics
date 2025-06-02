#' Rotate 3D Marker Coordinates in a Data Frame
#'
#' Applies a 3D rotation to all marker columns in a data frame. Marker columns are expected
#' to follow the naming pattern: `marker_X`, `marker_Y`, `marker_Z`.
#'
#' @param data A data frame containing 3D marker coordinates.
#' @param angle_deg Numeric. The rotation angle in degrees.
#' @param axis Character. The axis of rotation: "x", "y", or "z".
#' @param ignore_case Logical. Should column name matching of the suffix (e.g., "_X") ignore case? Default is TRUE.
#'
#' @return A data frame with rotated marker coordinates.
#' @export
rotate_markers <- function(data, angle_deg, axis, ignore_case = T) {
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.numeric(angle_deg) || length(angle_deg) != 1) {
    stop("`angle_deg` must be a single numeric value.")
  }
  if (!axis %in% c("x", "y", "z")) {
    stop("`axis` must be one of: 'x', 'y', or 'z'.")
  }
  
  # Create the rotation matrix based on the user-provided angle and axis
  rotation_matrix <- create_rotation_matrix(angle_deg, axis)
  
  # Find all columns that end with '_x', '_y', or '_z'
  marker_columns <- grep("_(X|Y|Z)$", names(data), value = TRUE, ignore.case = ignore_case)
  
  # Loop through each marker (groups of x, y, z columns)
  for (marker_base in unique(sub("_(X|Y|Z)$", "", marker_columns, ignore.case = ignore_case))) {
    # Get the x, y, and z columns for this marker
    x_col <- paste0(marker_base, "_X")
    y_col <- paste0(marker_base, "_Y")
    z_col <- paste0(marker_base, "_Z")
    
    # Rotate the coordinates for this marker
    rotated <- rotate_coords(data[[x_col]], data[[y_col]], data[[z_col]], rotation_matrix)
    
    # Assign the rotated coordinates back to the data frame
    data[[x_col]] <- rotated$x
    data[[y_col]] <- rotated$y
    data[[z_col]] <- rotated$z
  }
  return(data)
}