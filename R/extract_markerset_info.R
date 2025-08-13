#' Extract Marker Set Information from OpenSim marker set file
#'
#' This function extracts information about the markers in an OpenSim marker set file,
#' including their name, parent body, fixed status, and location. Optionally, the
#' results can be written to a CSV file.
#'
#' @param marker_file A character string specifying the path to the marker set XML file.
#' @param output_file A character string specifying the path to the output CSV file.
#' @param write_to_file A logical indicating whether to write the result to a CSV file. Default is `FALSE`.
#' @param return_obj A logical indicating whether to return the data frame as an object. Default is `TRUE`.
#'
#' @return A data frame containing the marker information.
#'
#' @import dplyr
#' @import reticulate
#' @import stringr
#'
#' @export
extract_markerset_info <- function(marker_file = "markerset.xml",
                           output_file = "marker_info.csv",
                           write_to_file = FALSE,
                           return_obj = TRUE) {

  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required. Please install it.")
  }
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required. Please install it.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The 'stringr' package is required. Please install it.")
  }

  # Check that the marker_file exists
  if (!file.exists(marker_file)) {
    stop("The specified marker file does not exist.")
  }

  # Load required Python packages using reticulate
  osim <- tryCatch({
    reticulate::import("opensim")
  }, error = function(e) {
    stop("Could not load the OpenSim Python package. Ensure OpenSim is installed.")
  })

  # Get marker set
  data_markers <- osim$MarkerSet(marker_file)

  # Get the size of the marker set
  marker_set_size <- data_markers$getSize()

  # Setup storage for marker information
  marker_set_data <- matrix(nrow = marker_set_size, ncol = 6)

  # Get information for each marker
  for (marker_i in 1L:marker_set_size) {

    # Adjust iterator for zero-based indexing in OpenSim API
    marker_i_L <- marker_i - 1L

    # Get marker object
    marker_i_obj <- data_markers$get(marker_i_L)

    # Extract marker info
    marker_i_name <- marker_i_obj$getName()
    marker_i_body <- marker_i_obj$getParentFrameName() %>% stringr::str_remove_all(., "/bodyset/")
    marker_i_fixed <- marker_i_obj$get_fixed()
    marker_loc_x <- marker_i_obj$get_location()$get(0L)
    marker_loc_y <- marker_i_obj$get_location()$get(1L)
    marker_loc_z <- marker_i_obj$get_location()$get(2L)

    # Store marker info in the matrix
    marker_set_data[marker_i, 1] <- marker_i_name
    marker_set_data[marker_i, 2] <- marker_i_body
    marker_set_data[marker_i, 3] <- marker_i_fixed
    marker_set_data[marker_i, 4] <- marker_loc_x
    marker_set_data[marker_i, 5] <- marker_loc_y
    marker_set_data[marker_i, 6] <- marker_loc_z
  }

  # Convert to data frame and add additional columns
  marker_set_df <- marker_set_data %>%
    as.data.frame() %>%
    `colnames<-`(c("marker_name", "body", "fixed", "location_x", "location_y", "location_z"))

  # Write to file if requested
  if (write_to_file) {
    write.csv(marker_set_df, output_file, row.names = FALSE)
  }

  # Return the data frame if requested
  if (return_obj) {
    return(marker_set_df)
  }
}
