#' Rotate an Opensim Data Table
#'
#' This function rotates a specified OpenSim data table around a given axis
#' by a specified angle in degrees.
#'
#' @param table_object An object representing an OpenSim data table.
#' @param axis A numeric vector of length 3 specifying the rotation axis (default is c(1, 0, 0)).
#' @param deg A numeric value specifying the rotation angle in degrees (default is -90).
#'
#' @return NULL. The function modifies the input `table_object` in place.
#' @export
#'
#' @import reticulate
#' @import pracma
rotate_osim_table <- function(table_object, 
                              axis = c(1, 0, 0), 
                              deg = -90) {
  
  # Check if reticulate and pracma are installed
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but is not installed.")
  }
  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop("The 'pracma' package is required but is not installed.")
  }
  
  # Load OpenSim via reticulate
  osim <- NULL
  tryCatch({
    osim <- reticulate::import("opensim")
  }, error = function(e) {
    stop("Failed to import 'opensim' module. Please ensure that the OpenSim Python package is installed and accessible.")
  })
  
  # Load OpenSim via reticulate
  osim <- reticulate::import("opensim")
  
  # Create rotation matrix
  R <- osim$Rotation(pracma::deg2rad(deg), osim$Vec3(axis[1], axis[2], axis[3]))
  
  # Iterate through table
  for (i_L in seq_len(table_object$getNumRows())) {
    
    # Get the row at a given index (adjust for zero-indexing)
    i <- i_L - 1
    
    # Get the row vector at index
    vec <- table_object$getRowAtIndex(i)
    
    # Rotate the vector
    vec_rotated <- R$multiply(vec)
    
    # Override the data
    table_object$setRowAtIndex(i, vec_rotated)
  }
  
  # Explicit return for clarity, need to test this
  return(NULL) 
}
