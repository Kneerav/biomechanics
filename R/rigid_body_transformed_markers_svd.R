#' Apply Rigid Body Transformation to Markers Using SVD
#'
#' This function applies a rigid body transformation to a set of target 3D markers
#' using singular value decomposition (SVD) based on a reference set of 3D markers.
#'
#' @param reference_data A matrix or data frame containing the reference 3D marker data
#'   (dimensions should be N x 3M, where N is the number of frames and M is the number of markers).
#' @param target_data A matrix or data frame containing the target 3D marker data
#'   (dimensions should be N x 3M, where N is the number of frames and M is the number of markers).
#'
#' @return A data frame containing the transformed 3D marker data for each frame.
#' @importFrom dplyr slice bind_rows %>%
#' @export
rigid_body_transformed_markers_svd = function(reference_data,
                                              target_data){


  # Check inputs
  if (!is.data.frame(reference_data) && !is.matrix(reference_data)) {
    stop("`reference_data` must be a data frame or matrix.")
  }
  if (!is.data.frame(target_data) && !is.matrix(target_data)) {
    stop("`target_data` must be a data frame or matrix.")
  }

  if (!all(ncol(reference_data) == ncol(target_data))) {
    stop("`reference_data` and `target_data` must have the same dimensions.")
  }

  if (ncol(reference_data) %% 3 != 0) {
    stop("Number of columns must be a multiple of 3 (X, Y, Z for each marker).")
  }

  # baseline info
  nframe = nrow(target_data)
  n_markers <- ncol(reference_data) / 3
  markerNames = colnames(reference_data)
  A <- matrix(as.numeric(reference_data), ncol = 3, byrow = TRUE)
  A_homo <- rbind(t(A), 1)

  #storage
  rigid_transformed_marker_list = list()

  #loop through frames of target data
  for(frame_i in 1:nframe){

    #get current target frame
    target_data_i = target_data %>%  slice(frame_i)

    #run svd
    frame_i_svd = rigid_transform_svd(reference_data = reference_data, target_data = target_data_i)

    #re-estimate B
    B_estimated <- frame_i_svd$T_mat %*% A_homo
    B_estimated_data = B_estimated[1:3, ]

    #format to match input df style
    df <- as.data.frame(t(as.vector(B_estimated_data))) %>%
      `colnames<-`(markerNames)

    #append to list
    rigid_transformed_marker_list[[frame_i]] = df


  }

  #bind to df
  rigid_transformed_marker_df = dplyr::bind_rows(rigid_transformed_marker_list)

  #return the rigid transformed data
  return(rigid_transformed_marker_df)

}
