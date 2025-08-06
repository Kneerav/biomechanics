#' Compute Joint Centre Using the SCoRE Method
#'
#' Computes the joint centre using the Symmetrical Centre of Rotation Estimation (SCoRE) method
#' based on time series of marker positions from proximal and distal segments.
#'
#' @param data A data frame with time-series marker data. Columns must be named in the format `marker_X`, `marker_Y`, `marker_Z`.
#' @param marker_names_prox A character vector of 3 marker names defining the proximal segment.
#' @param marker_names_dist A character vector of 3 marker names defining the distal segment.
#' @param joint_centre_name A character string used to name the resulting joint centre columns.
#' @param return_items Character string specifying what to return. One of `"all"`, `"local_distal"`, `"local_proximal"`, `"global"`, or `"appended"`.
#'
#' @return Depending on `return_items`:
#' * `"all"` – a list with full data frame, local distal, and local proximal coordinates.
#' * `"local_distal"` or `"local_proximal"` – a 3x1 numeric matrix.
#' * `"global"` – a data frame with joint centre coordinates in global frame.
#' * `"appended"` – original data with global joint centre columns appended.
#'
#' @references Ehrig RM, Taylor WR, Duda GN, Heller MO (2006). A survey of formal methods for determining the centre of rotation of joints. \emph{Journal of Biomechanics}, 39(15):2798–2809.
#'
#' @importFrom dplyr %>% bind_cols
#' @importFrom purrr pmap
#' @importFrom pracma inv
#' @export
compute_jc_score <- function(data,
                             marker_names_prox,
                             marker_names_dist,
                             joint_centre_name,
                             return_items = c("all",
                                              "local_distal",
                                              "local_proximal",
                                              "global",
                                              "appended")) {

  # Validate inputs
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (length(marker_names_prox) != 3 || length(marker_names_dist) != 3) {
    stop("`marker_names_prox` and `marker_names_dist` must each contain exactly 3 marker names.")
  }
  return_items <- match.arg(return_items)

  library(dplyr)

  #function to get marker vector
  get_marker_vector <- function(df_row, marker_name) {
    c(
      df_row[[paste0(marker_name, "_X")]],
      df_row[[paste0(marker_name, "_Y")]],
      df_row[[paste0(marker_name, "_Z")]]
    )
  }

  # Allocate matrices for b and c
  b_i <- matrix(0, nrow = 3 * nrow(data), ncol = 6)
  c_i <- matrix(0, nrow = 3 * nrow(data), ncol = 1)

  # Compute distal coordinate system
  t_d <- data %>%
    purrr::pmap(function(...) {
      row <- list(...)
      row <- as.data.frame(row, stringsAsFactors = FALSE)

      marker_1 <- get_marker_vector(row, marker_names_dist[1])
      marker_2 <- get_marker_vector(row, marker_names_dist[2])
      marker_3 <- get_marker_vector(row, marker_names_dist[3])

      compute_frame_generic(marker_1, marker_2, marker_3)
    })

  #each element is a 4x4 element (3x3 rotation and 3x1 translation)
  t_d = frames_array <- array(unlist(t_d), dim = c(4, 4, length(t_d)))

  # Compute proximal coordinate system
  t_p <- data %>%
    purrr::pmap(function(...) {
      row <- list(...)
      row <- as.data.frame(row, stringsAsFactors = FALSE)

      marker_1 <- get_marker_vector(row, marker_names_prox[1])
      marker_2 <- get_marker_vector(row, marker_names_prox[2])
      marker_3 <- get_marker_vector(row, marker_names_prox[3])

      compute_frame_generic(marker_1, marker_2, marker_3)
    })

  t_p = frames_array <- array(unlist(t_p), dim = c(4, 4, length(t_p)))

  #Loop through and assign the b_i (rotation) and c_i (translation) matrices
  for (i in 1:dim(t_p)[3]) {

    r_d <- t_d[1:3, 1:3, i]
    p_d <- t_d[ 1:3, 4, i]
    r_p <- t_p[1:3, 1:3, i]
    p_p <- t_p[1:3, 4, i]

    # Fill in matrices b and c
    b_rows <- (3 * (i - 1) + 1):(3 * i)
    b_i[b_rows, 1:3] <- r_d
    b_i[b_rows, 4:6] <- -r_p
    c_i[b_rows, 1] <- p_p - p_d

  }

  # Solve linear system: vu = (bᵀb)⁻¹ bᵀ c
  vu <- solve(t(b_i) %*% b_i) %*% t(b_i) %*% c_i

  #get gh in each frame
  g_distal <- vu[1:3, , drop = FALSE]
  g_proximal <- vu[4:6, , drop = FALSE]

  #set local vectors
  v_distal = g_distal
  v_proximal = g_proximal

  #convert distal back to global for averging, then return glob and distals
  g_global = list()
  g_distal_ls = list()
  g_proximal_ls = list()

  for (i in 1:dim(t_p)[3]) {

    ### Distal ###
    r_d <- t_d[1:3, 1:3, i] #3x3 rotation matrix (local to global)
    p_d <- t_d[ 1:3, 4, i] # p: 3x1 or length-3 vector (origin of local frame in global coords)

    # Multiply rotation matrix by local joint center vector
    ghr_local_dist <- as.vector(r_d %*% v_distal)  # ensure it returns a numeric vector

    # Add origin to transform to global coordinates
    ghr_global_dist <- ghr_local_dist + p_d
    #pracma::inv(r_d) %*% (ghr_global_dist - p_d) #should reproduce v_distal

    ### Proximal ###
    r_p <- t_p[1:3, 1:3, i] #3x3 rotation matrix (local to global)
    p_p <- t_p[ 1:3, 4, i] # p: 3x1 or length-3 vector (origin of local frame in global coords)

    # Multiply rotation matrix by local joint center vector
    ghr_local_prox <- as.vector(r_p %*% v_proximal)  # ensure it returns a numeric vector

    # Add origin to transform to global coordinates
    ghr_global_prox <- ghr_local_prox + p_p
    #pracma::inv(r_p) %*% (ghr_global_prox - p_p) #should reproduce v_distal

    #average the two estimates
    ghr_global = 0.5 * (ghr_global_prox + ghr_global_dist)

    ### Back to local ###

    #now re-express in local coordinate frame for distal segment
    v_distal_rev = pracma::inv(r_d) %*% (ghr_global - p_d)
    v_proximal_rev = pracma::inv(r_p) %*% (ghr_global - p_p)

    # Fill in matrices b and c
    g_global[[i]] = ghr_global
    g_distal_ls[[i]] = v_distal_rev
    g_proximal_ls[[i]] = v_proximal_rev

  }

  #get global coord df for appending
  g_global_df = do.call("rbind.data.frame", g_global) %>%
    `colnames<-`(paste0(joint_centre_name, c("_X", "_Y", "_Z")))

  #get point in distal
  g_matrix_dist <- do.call(cbind, g_distal_ls)  # result: 3 x N matrix
  g_mean_dist <- matrix(rowMeans(g_matrix_dist), nrow=3, ncol=1 )  # result: 3x1 averaged matrix

  #get point in distal
  g_matrix_prox <- do.call(cbind, g_proximal_ls)  # result: 3 x N matrix
  g_mean_prox <- matrix(rowMeans(g_matrix_prox), nrow=3, ncol=1 )  # result: 3x1 averaged matrix

  #check return items. local need to be reformatted
  if(return_items == "all"){
    g_global_df_appended = cbind.data.frame(data, g_global_df)
    return_obj = list(data = g_global_df_appended, g_distal = g_mean_dist, g_proximal = g_mean_prox)
  } else if(return_items == "local_distal"){
    return_obj = g_mean_dist
  } else if(return_items == "local_proximal"){
    return_obj =  g_mean_prox
  } else if(return_items == "global"){
    return_obj = g_global_df
  } else if(return_items == "appended"){
    g_global_df_appended = cbind.data.frame(data, g_global_df)
    return_obj = g_global_df_appended
  } else {
    stop("Invalid return_items specified. Choose either 'all', 'local_distal', 'local_proximal', 'global' or 'appended'.")
  }

  #return
  return(return_obj)
}
