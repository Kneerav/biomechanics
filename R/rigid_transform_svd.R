#' Rigid Body Transformation (Söderkvist & Wedin Method) Between Two Marker Frames
#'
#' Computes the 4x4 rigid body transformation matrix between two frames of 3D marker data,
#' using the SVD method of Söderkvist & Wedin (1993).
#'
#' @param reference_data A data frame or matrix representing the reference frame (e.g., time 1).
#' @param target_data A data frame or matrix representing the target frame (e.g., time 2).
#'
#' @return A list containing:
#' \describe{
#'   \item{T}{4x4 transformation matrix (rotation and translation)}
#'   \item{res}{Residual error (Euclidean norm adjusted by DOF)}
#' }
#'
#' @references Söderkvist & Wedin (1993). Determining the movements of the skeleton using well-configured markers.
#'            Journal of Biomechanics, 26:1473–1477.
#'
#' @importFrom pracma Rank
#'
#' @export
rigid_transform_svd <- function(reference_data, target_data) {
  # Check inputs
  if (!is.data.frame(reference_data) && !is.matrix(reference_data)) {
    stop("`reference_data` must be a data frame or matrix.")
  }
  if (!is.data.frame(target_data) && !is.matrix(target_data)) {
    stop("`target_data` must be a data frame or matrix.")
  }

  reference_data <- as.matrix(reference_data) # 1 x N_marker matrix
  target_data <- as.matrix(target_data) # 1 x N_marker matrix

  if (!all(dim(reference_data) == dim(target_data))) {
    stop("`reference_data` and `target_data` must have the same dimensions.")
  }

  if (ncol(reference_data) %% 3 != 0) {
    stop("Number of columns must be a multiple of 3 (X, Y, Z for each marker).")
  }


  # Reshape: each marker is a row, columns are X Y Z
  A <- matrix(reference_data, ncol = 3, byrow = TRUE)
  B <- matrix(target_data, ncol = 3, byrow = TRUE)

  # Remove rows with any NaNs in A or B
  valid <- !(apply(A, 1, function(x) any(is.na(x))) |
               apply(B, 1, function(x) any(is.na(x))))
  A <- A[valid, , drop = FALSE]
  B <- B[valid, , drop = FALSE]

  # Check that at least 3 markers remain
  if (nrow(A) < 3) {
    T_mat <- matrix(NA_real_, 4, 4)
    return(list(T_mat = T_mat, res = NA_real_))
  }

  #get basic dimensions
  n_markers <- ncol(reference_data) / 3
  ncol_A <- ncol(A)

  # Compute means
  A_mean <- colMeans(A)
  B_mean <- colMeans(B)

  # mean matrix
  A_mean_matrix <- matrix(A_mean, nrow=1, ncol=ncol_A)
  B_mean_matrix <- matrix(B_mean, nrow=1, ncol=ncol_A)

  # Subtract means, reshape to matching matrix size
  A_centred <- A - matrix(A_mean_matrix, nrow=nrow(A), ncol=ncol_A, byrow=TRUE)
  B_centred <- B - matrix(B_mean_matrix, nrow=nrow(B), ncol=ncol_A, byrow=TRUE)
  #(transposing just ensures correct maths, then transpose back to original config)
  #Ai <- t(t(A) - A_mean)
  #Bi <- t(t(B) - B_mean)

  # transpose ( or could leave and compute C = t(A_centred) %*% B_centred, and then R = Q %*% t(P))
  Ai <- t(A_centred)
  Bi <- t(B_centred)

  # Cross-covariance matrix
  C <- Bi %*% t(Ai)

  #Check matrix rank
  rank_i <- pracma::Rank(C)[1] #3

  if (ncol_A == 2 && rank_i == 0) {
    stop(sprintf("Insufficient matrix rank. For 2D points expect rank >= 1 but got %d. Maybe your points are all the same?", rank))
  } else if (ncol_A == 3 && rank_i <= 1) {
    stop(sprintf("Insufficient matrix rank. For 3D points expect rank >= 2 but got %d. Maybe your points are collinear?", rank))
  }

  #svd
  svd_res <- svd(C)
  P <- svd_res$u
  Q <- svd_res$v

  # Proper rotation matrix (u %*% t(v))
  R <- P %*% diag(c(1, 1, det(P %*% t(Q)))) %*% t(Q)

  # Handle reflection case
  det_R <- det(R)
  if (det_R < 0) {
    cat(sprintf("det(R) = %f, reflection detected!, correcting for it ...\n", det_R))
    S <- diag(ncol_A)
    S[ncol_A, ncol_A] <- -1
    R <- P %*% S %*% t(Q)
  }

  # Translation vector
  d <- B_mean - (R %*% A_mean)

  # Homogeneous transformation matrix
  T_mat <- matrix(0, 4, 4)
  T_mat[1:3, 1:3] <- R
  T_mat[1:3, 4] <- d
  T_mat[4, 4] <- 1

  # Residuals
  A_homo <- t(A)
  A_homo <- rbind(A_homo, 1)  # add row of 1s
  B_calc <- T_mat %*% A_homo
  B_calc <- B_calc[1:3, , drop = FALSE]
  Diff <- t(B) - B_calc
  DOF <- prod(dim(Diff)) - 6  # 6 rigid body DOF
  res <- sqrt(sum(Diff^2) / DOF)

  return(list(T_mat = T_mat, res = res))
}
