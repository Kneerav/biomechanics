#' Convert Rotation Vector to Rotation Matrix
#'
#' Converts a 3-element rotation vector (axis-angle representation) into a 3x3 rotation matrix,
#' using Rodrigues' rotation formula. Equivalent to Python's
#' \code{scipy.spatial.transform.Rotation.from_rotvec}.
#'
#' @param theta3 A numeric vector of length 3, representing the rotation vector. Its direction
#'   specifies the axis of rotation, and its magnitude specifies the angle (in radians by default).
#' @param in_degrees Logical. If \code{TRUE}, the elements of \code{theta3} are interpreted as
#'   degrees and converted to radians internally. Default is \code{FALSE}.
#'
#' @return A 3x3 numeric matrix representing the rotation.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Axis-angle_representation#Rotation_vector}
#' \url{https://docs.scipy.org/doc/scipy/reference/generated/scipy.spatial.transform.Rotation.from_rotvec.html}
#'
#' @examples
#' create_rotation_matrix_from_vector(c(0.5, 0.4, 0.3))
#' create_rotation_matrix_from_vector(c(30, 0, 0), in_degrees = TRUE)
#'
#' @export
create_rotation_matrix_from_vector <- function(theta3, in_degrees = FALSE) {

  # Ensure theta3 is a 3-element vector
  if (length(theta3) != 3) {
    stop("Input vector must have exactly 3 elements.")
  }

  #Convert to radians if needed
  if(in_degrees){
    theta3 <- theta3 * pi/180
  }

  #Calculate the angle (magnitude of the vector) and axis of rotation
  angle <- sqrt(sum(theta3^2))  # Angle of rotation (magnitude of the vector)

  #If the angle is close to zero, return the identity matrix (no rotation)
  if (angle == 0) {
    return(diag(3))
  }

  #Normalize the rotation vector to get the unit axis of rotation
  axis <- theta3 / angle

  #Calculate the components of the rotation matrix using the Rodrigues' rotation formula
  cos_theta <- cos(angle)
  sin_theta <- sin(angle)

  #Cross-product matrix of the axis
  K <- matrix(c(0, -axis[3], axis[2],
                axis[3], 0, -axis[1],
                -axis[2], axis[1], 0),
              nrow = 3, byrow = TRUE)

  #Rotation matrix: R = I + sin(θ) * K + (1 - cos(θ)) * K^2
  R <- diag(3) + sin_theta * K + (1 - cos_theta) * (K %*% K)

  return(R)
}
