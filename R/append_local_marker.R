#' Append a Marker Defined in a Local Coordinate System to Global Data
#'
#' Transforms a marker defined in a local reference frame (defined by 3 markers)
#' into the global coordinate system for each row in the dataset, and appends it to the data.
#'
#' @param data A data frame of marker data, with columns named in the format `marker_X`, `marker_Y`, `marker_Z`.
#' @param marker_local A numeric vector of length 3 representing the local coordinates (X, Y, Z) of the marker.
#' @param frame_markers A character vector of 3 marker names that define the local coordinate system.
#'                      The second marker is used as the origin.
#' @param local_marker_name A string giving the name of the local marker to be added.
#' @param append Logical. If `TRUE` (default), appends the marker to the original data.
#'               If `FALSE`, returns only the transformed marker positions.
#'
#' @return A data frame. If `append = TRUE`, returns the original `data` with the new marker columns appended.
#' If `FALSE`, returns a data frame with just the transformed marker.
#'
#' @importFrom purrr pmap
#' @importFrom dplyr bind_cols %>%
#' @export
append_local_marker = function(data,
                               marker_local,
                               frame_markers,
                               local_marker_name,
                               append = TRUE){

  # marker_local is a 3 x 1 numeric object contain the local x,y,z coordinates
  # frame_marker is a 3 element character vector containing the marker names to define the local reference frame (must be exactly the same markers and order as used for the local marker), note that the second marker for frame_marker is auto designated as the origin
  # local_marker_name is a character variable defining the name of the marker (e.g., L.ASIS)

  # Validate inputs
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (length(marker_local) != 3 || !is.numeric(marker_local)) {
    stop("`marker_local` must be a numeric vector of length 3.")
  }
  if (length(frame_markers) != 3 || !is.character(frame_markers)) {
    stop("`frame_markers` must be a character vector of length 3.")
  }
  if (!is.character(local_marker_name) || length(local_marker_name) != 1) {
    stop("`local_marker_name` must be a single character string.")
  }

  #function to get marker vector
  get_marker_vector <- function(df_row, marker_name) {
    c(
      df_row[[paste0(marker_name, "_X")]],
      df_row[[paste0(marker_name, "_Y")]],
      df_row[[paste0(marker_name, "_Z")]]
    )
  }

  # Compute local coordinate system
  t_d <- data %>%
    purrr::pmap(function(...) {
      row <- list(...)
      row <- as.data.frame(row, stringsAsFactors = FALSE)

      marker_1 <- get_marker_vector(row, marker_names_dist[1])
      marker_2 <- get_marker_vector(row, marker_names_dist[2])
      marker_3 <- get_marker_vector(row, marker_names_dist[3])

      compute_frame_generic(marker_1, marker_2, marker_3)
    })

  t_d = frames_array <- array(unlist(t_d), dim = c(4, 4, length(t_d)))

  #define vector to be transformed
  v = marker_local

  g_global = list()
  for (i in 1:nrow(data)) {

    r_h <- t_d[1:3, 1:3, i] #3x3 rotation matrix (local to global)
    p_h <- t_d[ 1:3, 4, i] # p: 3x1 or length-3 vector (origin of local frame in global coords)

    # Multiply rotation matrix by local joint center vector
    ghr_local <- as.vector(r_h %*% v)  # ensure it returns a numeric vector

    # Add origin to transform to global coordinates
    ghr_global <- ghr_local + p_h

    # Fill in matrices b and c
    g_global[[i]] = ghr_global

  }

  #need naming arguments
  g_global_df = do.call("rbind.data.frame", g_global) %>%
    `colnames<-`(paste0(local_marker_name, c("_X", "_Y", "_Z")))

  #check return items. local need to be reformatted
  if(append){
    g_global_df_appended = cbind.data.frame(data, g_global_df)
    return_obj = g_global_df_appended
  } else {
    return_obj = g_global_df
  }

  return(return_obj)

}
