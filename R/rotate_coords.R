#' Rotate 3D Coordinates Using a Rotation Matrix
#'
#' Applies a 3x3 rotation matrix to a set of 3D coordinates.
#'
#' @param x, y, z Numeric vectors of equal length representing 3D coordinates.
#' @param rotation_matrix A 3x3 numeric matrix used to rotate the coordinates.
#'
#' @return A data frame with rotated coordinates (`x`, `y`, `z`).
#' @examples
#' rot_mat <- create_rotation_matrix(90, "z")
#' rotate_coords(1, 0, 0, rot_mat)
#'
#' @export
rotate_coords <- function(x, y, z, rotation_matrix) {
  
  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(z)) {
    stop("x, y, and z must be numeric vectors.")
  }
  if (!(is.matrix(rotation_matrix) && all(dim(rotation_matrix) == c(3, 3)))) {
    stop("rotation_matrix must be a 3x3 numeric matrix.")
  }
  if (!(length(x) == length(y) && length(y) == length(z))) {
    stop("x, y, and z must have the same length.")
  }
  
  coords <- cbind(x, y, z) %*% rotation_matrix
  
  return(data.frame(x = coords[, 1], y = coords[, 2], z = coords[, 3]))
  
}