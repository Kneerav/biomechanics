#' Analyze Knee Compartment Forces
#'
#' This function calculates the medial and lateral forces at the knee based on input force and moment data.
#' It computes the forces using the provided force and moment data and applies the Moore-Penrose pseudo-inverse to solve for the forces in the medial and lateral compartments.
#'
#' @param jr_data A data frame containing joint reaction forces and moments.
#' @param fx_col Name of the column containing the x-component of the force.
#' @param fy_col Name of the column containing the y-component of the force.
#' @param fz_col Name of the column containing the z-component of the force.
#' @param mx_col Name of the column containing the x-component of the moment.
#' @param my_col Name of the column containing the y-component of the moment.
#' @param mz_col Name of the column containing the z-component of the moment.
#' @param r_med A numeric vector of length 3 representing the medial vector (default is `c(0,0,-0.02)`).
#' @param r_lat A numeric vector of length 3 representing the lateral vector (default is `c(0,0, 0.02)`).
#' @param fmed_name The prefix for the columns naming the medial forces (default is `"fmed_r"`).
#' @param flat_name The prefix for the columns naming the lateral forces (default is `"flat_r"`).
#' @param append Logical value indicating whether to append the calculated forces to the input data frame (`TRUE` by default).
#' @return A data frame containing the original input data along with the calculated medial and lateral forces (if `append = TRUE`), or just the calculated forces (if `append = FALSE`).
#' @import dplyr
#' @importFrom pracma pinv
#' @export
analyse_knee_compartment_forces <- function(
    jr_data,
    fx_col = "walker_knee_r_on_tibia_r_in_tibia_r_fx",
    fy_col = "walker_knee_r_on_tibia_r_in_tibia_r_fy",
    fz_col = "walker_knee_r_on_tibia_r_in_tibia_r_fz",
    mx_col = "walker_knee_r_on_tibia_r_in_tibia_r_mx",
    my_col = "walker_knee_r_on_tibia_r_in_tibia_r_my",
    mz_col = "walker_knee_r_on_tibia_r_in_tibia_r_mz",
    r_med = c(0, 0, -0.02),
    r_lat = c(0, 0, 0.02),
    fmed_name = "fmed_r",
    flat_name = "flat_r",
    append = TRUE) {

  # Ensure required packages are available
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it with install.packages('dplyr').")
  }
  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop("Package 'pracma' is required. Please install it with install.packages('pracma').")
  }

  # Extract force and moment data
  fx_df <- jr_data[, fx_col]
  fy_df <- jr_data[, fy_col]
  fz_df <- jr_data[, fz_col]

  mx_df <- jr_data[, mx_col]
  my_df <- jr_data[, my_col]
  mz_df <- jr_data[, mz_col]

  # Setup lists for storing results
  f_med_list <- list()
  f_lat_list <- list()

  for (row_i in 1:nrow(jr_data)) {

    # Reaction force and moment vectors
    f_react <- c(fx_df[row_i], fy_df[row_i], fz_df[row_i])
    m_react <- c(mx_df[row_i], my_df[row_i], mz_df[row_i])

    # Identity matrix
    I3 <- diag(3)

    # Cross product matrices
    r_med_cross <- create_cross_mat_from_vector(r_med)
    r_lat_cross <- create_cross_mat_from_vector(r_lat)

    # Construct A_m matrix
    A_m <- rbind(
      cbind(I3, I3),
      cbind(r_med_cross, r_lat_cross)
    )

    # Construct b_m (constants) vector
    b_m <- c(f_react, m_react)

    # Solve for x_m using Moore-Penrose pseudo-inverse
    x_m <- pracma::pinv(A_m) %*% b_m

    # Extract results
    f_med_list[[row_i]] <- x_m[1:3] %>% t() %>% as.data.frame() %>% `colnames<-`(paste(fmed_name, c("x", "y", "z"), sep = "_"))
    f_lat_list[[row_i]] <- x_m[4:6] %>% t() %>% as.data.frame() %>% `colnames<-`(paste(flat_name, c("x", "y", "z"), sep = "_"))

  }

  # Combine results
  f_med <- bind_rows(f_med_list)
  f_lat <- bind_rows(f_lat_list)

  # Return the results
  if (append) {
    return(cbind(jr_data, f_med, f_lat))
  } else {
    return(cbind(f_med, f_lat))
  }
}
