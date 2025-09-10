#' Compute a Reference Frame from Three Non-Colinear Markers
#'
#' Constructs a 4x4 transformation matrix representing a reference frame
#' defined by three non-colinear 3D points. The frame is based on:
#' - Origin: marker_2
#' - X-axis: from marker_2 to marker_3
#' - Y-axis: from marker_2 to marker_1
#' - Z-axis: orthogonal to the XY-plane (via cross product)
#' Note that x is re-computed via cross product of y and z to make the frame orthogonal
#'
#' @param marker_1 A numeric vector of length 3, representing a 3D point.
#' @param marker_2 A numeric vector of length 3, representing a 3D point (used as origin).
#' @param marker_3 A numeric vector of length 3, representing a 3D point.
#'
#' @return A 4x4 numeric matrix representing a homogeneous transformation matrix.
#' @importFrom pracma cross
#' @export
#'
#' @examples
#' compute_frame_generic(c(1, 0, 0), c(0, 0, 0), c(0, 1, 0))
compute_frame_generic <- function(marker_1, marker_2, marker_3) {

  # Validate inputs
  if (!all(length(marker_1) == 3, length(marker_2) == 3, length(marker_3) == 3)) {
    stop("All marker inputs must be numeric vectors of length 3.")
  }

  # Set marker 2 as origin
  o <- marker_2

  # Define X-axis: from marker_2 to marker_3
  x <- marker_3 - marker_2
  x <- x / sqrt(sum(x^2))  # Normalize

  # Define Y-axis: from marker_2 to marker_1
  y <- marker_1 - marker_2
  y <- y / sqrt(sum(y^2))  # Normalize

  # Define Z-axis as cross product of X and Y
  z <- pracma::cross(x, y)  # Use 'pracma' package for cross product
  z <- z / sqrt(sum(z^2))   # Normalize

  # recompute x
  x_orth <- pracma::cross(y,z)
  x_orth <- x_orth / sqrt(sum(x_orth^2))   # Normalize

  # Build 4x4 transformation matrix (rotation + origin)
  t_l <- rbind(
    cbind(x_orth, y, z, o),
    c(0, 0, 0, 1)
  )

  return(t_l)
}
