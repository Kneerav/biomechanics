#' Create a 3D Rotation Matrix
#'
#' Generates a 3x3 rotation matrix for rotating points in 3D space
#' around the x, y, or z axis by a specified angle.
#' Note that the result are standard active rotation matrices, meaning
#' they rotate vectors within a fixed frame using the right-hand rule.
#'
#' @param angle Numeric. The rotation angle.
#' @param axis Character. The axis of rotation: "x", "y", or "z".
#' @param in_degrees Logical. If TRUE (default), the input angle is assumed
#'   to be in degrees and is converted to radians. If FALSE, the angle is
#'   assumed to already be in radians.
#'
#' @return A 3x3 numeric matrix representing the rotation.
#'
#' @examples
#' create_rotation_matrix(90, "z")                # degrees (default)
#' create_rotation_matrix(pi/2, "z", FALSE)       # radians
#'
#' @export
create_rotation_matrix <- function(angle, axis, in_degrees = TRUE) {

  if (!is.numeric(angle) || length(angle) != 1) {
    stop("angle must be a single numeric value.")
  }

  if (!is.logical(in_degrees) || length(in_degrees) != 1) {
    stop("`in_degrees` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!axis %in% c("x", "y", "z")) {
    stop("Invalid axis. Choose 'x', 'y', or 'z'.")
  }

  #convert angle to rad
  angle_rad <- if (in_degrees) angle * pi / 180 else angle

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
