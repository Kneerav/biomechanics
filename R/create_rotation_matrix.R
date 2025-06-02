#' Create a 3D Rotation Matrix
#'
#' Generates a 3x3 rotation matrix for rotating points in 3D space
#' around the x, y, or z axis by a specified angle in degrees.
#'
#' @param angle_deg Numeric. The rotation angle in degrees.
#' @param axis Character. The axis of rotation: "x", "y", or "z".
#'
#' @return A 3x3 numeric matrix representing the rotation.
#' @examples
#' create_rotation_matrix(90, "z")
#'
#' @export
create_rotation_matrix <- function(angle_deg, axis) {
  
  if (!is.numeric(angle_deg) || length(angle_deg) != 1) {
    stop("angle_deg must be a single numeric value.")
  }
  if (!axis %in% c("x", "y", "z")) {
    stop("Invalid axis. Choose 'x', 'y', or 'z'.")
  }
  
  #convert angle to rad
  angle_rad <- angle_deg * pi / 180
  
  if (axis == "x") {
    rotation_matrix <- matrix(c(1, 0, 0,
                                0, cos(angle_rad), -sin(angle_rad),
                                0, sin(angle_rad), cos(angle_rad)), nrow = 3, byrow = TRUE)
  } else if (axis == "y") {
    rotation_matrix <- matrix(c(cos(angle_rad), 0, sin(angle_rad),
                                0, 1, 0,
                                -sin(angle_rad), 0, cos(angle_rad)), nrow = 3, byrow = TRUE)
  } else if (axis == "z") {
    rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad), 0,
                                sin(angle_rad), cos(angle_rad), 0,
                                0, 0, 1), nrow = 3, byrow = TRUE)
  } else {
    stop("Invalid axis. Choose 'x', 'y', or 'z'.")
  }
  return(rotation_matrix)
}