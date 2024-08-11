#' Compute Frame Orientation
#'
#' This function computes frame orientation based on two input vectors.
#'
#' @param v1 A numeric matrix or data frame where each row represents a vector.
#' @param v2 A numeric matrix or data frame where each row represents a vector.
#' @param recompute An integer indicating whether to recompute the orientation:
#'   \itemize{
#'     \item 1: Recompute u1 as the cross product of u2 and u3.
#'     \item 2: Recompute u2 as the cross product of u3 and u1.
#'   }
#' @param frameorder A numeric vector of length 3 specifying the order of the frames.
#'
#' @return A 3-dimensional array where each slice corresponds to a 3x3 orientation matrix.
#' @export
#'
#' @examples
#' v1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
#' v2 <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3, byrow = TRUE)
#' recompute <- 1
#' frameorder <- c(1, 2, 3)
#' compute_frame_orientation(v1, v2, recompute, frameorder)
compute_frame_orientation <- function(v1, v2, recompute, frameorder) {
  
  require(pracma)
  
  # Ensure v1 and v2 are data frames, then convert to matrices
  if (is.data.frame(v1)) v1 <- as.matrix(v1)
  if (is.data.frame(v2)) v2 <- as.matrix(v2)
  
  # Initialize the result array
  num_rows <- nrow(v1)
  frame <- array(0, dim = c(3, 3, num_rows))
  
  for (i in 1:num_rows) {
    # Extract row vectors
    u1 <- v1[i, ] / norm(v1[i, ], "2")
    u2 <- v2[i, ] / norm(v2[i, ], "2")
    
    # Compute v3 from the cross product of u1 and u2
    v3 <- pracma::cross(u1, u2)
    u3 <- v3 / norm(v3, "2")
    
    # Recompute based on the recompute parameter
    if (recompute == 1) {
      u1 <- pracma::cross(u2, u3)
    } else if (recompute == 2) {
      u2 <- pracma::cross(u3, u1)
    }
    
    # Normalize the vectors
    e1 <- u1 / norm(u1, "2")
    e2 <- u2 / norm(u2, "2")
    e3 <- u3 / norm(u3, "2")
    
    # Create the transformation matrix
    j <- c(e1, e2, e3)
    q <- matrix(0, nrow = 1, ncol = 9)
    
    # Map the columns from j to q based on frameorder
    q[, (frameorder[1]*3-2):(frameorder[1]*3)] <- j[1:3]
    q[, (frameorder[2]*3-2):(frameorder[2]*3)] <- j[4:6]
    q[, (frameorder[3]*3-2):(frameorder[3]*3)] <- j[7:9]
    
    # Reshape into the 3x3 matrix and store in the array
    frame[ , , i] <- matrix(q, nrow = 3, byrow = TRUE)
  }
  
  return(frame)
}
