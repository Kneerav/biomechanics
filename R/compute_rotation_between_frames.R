#' Compute Rotation Between Frames
#'
#' This function calculates the rotation matrix between corresponding frames in two 3D arrays.
#'
#' @param parent A 3-dimensional array where each slice represents a frame of rotation matrices.
#' @param child A 3-dimensional array of the same dimensions as `parent`, representing another set of rotation matrices.
#'
#' @return A 3-dimensional array of the same dimensions as `parent` and `child`, where each slice represents the rotation matrix between corresponding frames.
#' @export
#'
#' @examples
#' # Define two 3-dimensional arrays of rotation matrices
#' parent <- array(c(
#'   1, 0, 0, 0, 1, 0, 0, 0, 1,  # Frame 1
#'   0, -1, 0, 1, 0, 0, 0, 0, 1,  # Frame 2
#'   0, 0, 1, 0, 0, 0, -1, 0, 0   # Frame 3
#' ), dim = c(3, 3, 3))
#' child <- array(c(
#'   1, 0, 0, 0, 1, 0, 0, 0, 1,  # Frame 1
#'   0, 1, 0, -1, 0, 0, 0, 0, 1,  # Frame 2
#'   0, 0, -1, 0, 0, 0, 1, 0, 0   # Frame 3
#' ), dim = c(3, 3, 3))
#' compute_rotation_between_frames(parent, child)
compute_rotation_between_frames <- function(parent, child) {
  
  # Determine the number of frames
  n <- dim(parent)[3]
  
  # Initialize the result array
  c <- array(0, dim = dim(child))
  
  # Loop through each frame and calculate the rotation matrix
  for (i in 1:n) {
    c[,,i] <- child[,,i] %*% solve(parent[,,i])
  }
  
  return(c)
}