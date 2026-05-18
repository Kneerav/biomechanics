#' Compute frame angular velocity from rotation matrices
#'
#' Computes angular velocity from a time series of rotation matrices using
#' finite differences. Central differences are used for interior frames,
#' and either first- or second-order schemes are used at the endpoints.
#'
#' @param frame_array A 3 x 3 x N numeric array of rotation matrices.
#'   Each slice \code{frame_array[,,i]} must be a valid rotation matrix
#'   representing the orientation of the segment in the global frame.
#' @param dt Numeric scalar. Time step between frames (seconds).
#' @param endpoints Character string specifying endpoint method:
#'   \describe{
#'     \item{"first"}{First-order forward/backward difference (default)}
#'     \item{"second"}{Second-order forward/backward difference}
#'   }
#' @param enforce_skew Logical. If \code{TRUE}, forces the angular velocity
#'   matrix to be skew-symmetric before extracting the vector. Recommended
#'   when working with noisy data.
#'
#' @return A list with:
#' \itemize{
#'   \item{\code{frame_velocity}}{3 x 3 x N array of angular velocity matrices}
#'   \item{\code{omega}}{N x 3 matrix of angular velocity vectors (global frame)}
#' }
#'
#' @details
#' Angular velocity is computed as:
#' \deqn{\Omega = \dot{R} R^T}
#' where \eqn{R} is the rotation matrix. The angular velocity vector is
#' extracted from the skew-symmetric matrix \eqn{\Omega}.
#'
#' All inputs and outputs are expressed in the global (inertial) frame.
#'
#' @export
compute_frame_angular_velocity <- function(frame_array, dt,
                                           endpoints = c("first", "second"),
                                           enforce_skew = TRUE) {

  # --- argument checking ---
  if (!is.numeric(frame_array) || length(dim(frame_array)) != 3 ||
      dim(frame_array)[1] != 3 || dim(frame_array)[2] != 3) {
    stop("`frame_array` must be a 3 x 3 x N numeric array.", call. = FALSE)
  }

  if (!is.numeric(dt) || length(dt) != 1 || dt <= 0) {
    stop("`dt` must be a positive numeric scalar.", call. = FALSE)
  }

  endpoints <- match.arg(endpoints)

  n <- dim(frame_array)[3]
  if (n < 3) {
    stop("At least 3 frames are required.", call. = FALSE)
  }

  #storage
  v_array <- array(0, dim = c(3, 3, n))
  omega_matrix <- matrix(0, n, 3)

  #helper: vee operator
  vee <- function(Omega) {
    c(Omega[3,2], Omega[1,3], Omega[2,1])
  }

  #enforce skew symmetry
  make_skew <- function(Omega) {
    0.5 * (Omega - t(Omega))
  }

  #central differences
  for (i in 2:(n - 1)) {

    R_prev <- frame_array[,,i - 1]
    R_curr <- frame_array[,,i]
    R_next <- frame_array[,,i + 1]

    Rdot <- (R_next - R_prev) / (2 * dt)
    Omega <- Rdot %*% t(R_curr)

    if (enforce_skew) {
      Omega <- make_skew(Omega)
    }

    v_array[,,i] <- Omega
    omega_matrix[i, ] <- vee(Omega)
  }

  #endpoints
  if (endpoints == "first") {

    #forward difference
    Rdot_1 <- (frame_array[,,2] - frame_array[,,1]) / dt
    Omega_1 <- Rdot_1 %*% t(frame_array[,,1])

    #backward difference
    Rdot_n <- (frame_array[,,n] - frame_array[,,n - 1]) / dt
    Omega_n <- Rdot_n %*% t(frame_array[,,n])

  } else if (endpoints == "second") {

    #second-order forward
    Rdot_1 <- (-3 * frame_array[,,1] +
                 4 * frame_array[,,2] -
                 frame_array[,,3]) / (2 * dt)
    Omega_1 <- Rdot_1 %*% t(frame_array[,,1])

    #second-order backward
    Rdot_n <- (3 * frame_array[,,n] -
                 4 * frame_array[,,n - 1] +
                 frame_array[,,n - 2]) / (2 * dt)
    Omega_n <- Rdot_n %*% t(frame_array[,,n])
  }

  if (enforce_skew) {
    Omega_1 <- make_skew(Omega_1)
    Omega_n <- make_skew(Omega_n)
  }

  v_array[,,1] <- Omega_1
  v_array[,,n] <- Omega_n

  omega_matrix[1, ] <- vee(Omega_1)
  omega_matrix[n, ] <- vee(Omega_n)

  return(list(
    frame_velocity = v_array,
    omega = omega_matrix
  ))
}
