#' Convert a Vector to a Cross Product Matrix
#'
#' Given a 3D vector `r`, this function computes the corresponding cross product matrix.
#'
#' @param r A numeric vector of length 3, representing the 3D vector.
#' @return A 3x3 numeric matrix representing the cross product matrix of the input vector.
#' @export
create_cross_mat_from_vector <- function(r) {
  # Ensure r is a numeric vector of length 3
  if (length(r) != 3 || !is.numeric(r)) {
    stop("Input must be a numeric vector of length 3.")
  }

  # Create the cross product matrix
  matrix(c(  0,   -r[3],  r[2],
             r[3],   0,   -r[1],
             -r[2], r[1],    0),
         nrow = 3, byrow = TRUE)
}
