#' Compute Joint Angles from Rotation Frames
#'
#' This function computes Euler angles from a series of 3x3 rotation matrices.
#'
#' @param rotation_frames A 3-dimensional array of size 3x3xN, where each 3x3 slice represents a rotation matrix.
#'
#' @return A matrix of size N x 3, where each row contains the Euler angles (in degrees) corresponding to a 3x3 rotation matrix.
#' @export
#'
#' @import RSpincalc
#' @importFrom pracma rad2deg
#'
#' @examples
#' # Create a 3x3x2 array of rotation matrices
#' rotation_frames <- array(c(
#'   1, 0, 0, 0, 1, 0, 0, 0, 1,  # Matrix 1
#'   0, -1, 0, 1, 0, 0, 0, 0, 1,  # Matrix 2
#'   0, 0, 1, 0, 0, 0, -1, 0, 0   # Matrix 3
#' ), dim = c(3, 3, 3))
#' compute_joint_angles(rotation_frames)
compute_joint_angles <- function(rotation_frames) {
  
  require(RSpincalc)
  
  # Ensure the input is a 3-dimensional array
  if (length(dim(rotation_frames)) != 3 || dim(rotation_frames)[1] != 3 || dim(rotation_frames)[2] != 3) {
    stop("Input must be a 3x3xN array")
  }
  
  # Number of 3x3 matrices
  num_matrices <- dim(rotation_frames)[3]
  
  # Initialize matrix to store Euler angles
  all_eular_angles <- matrix(0, nrow = num_matrices, ncol = 3)
  
  for (i in 1:num_matrices) {
    # Extract the 3x3 matrix
    frame_matrix <- rotation_frames[, , i]
    
    # Compute Euler angles
    eular_angles <- RSpincalc::DCM2EA(frame_matrix, "zyx") %>% 
      pracma::rad2deg()
    
    # Adjust angles if needed (e.g., angles > 180 degrees)
    # This is a simple example; adjust based on your specific requirements
    eular_angles[eular_angles > 180] <- eular_angles[eular_angles > 180] - 360
    
    # Store angles
    all_eular_angles[i, ] <- eular_angles
  }
  
  return(all_eular_angles)
}