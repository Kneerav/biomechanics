#' Get Analog Data from C3D File
#'
#' This function reads a C3D file and extracts the analog data, 
#' optionally writing it to a .sto file and/or returning the 
#' opensim data table as an object.
#'
#' @param file_input A string specifying the path to the input C3D file.
#' @param file_output A string specifying the path for the output .sto file (default is "Analog.sto").
#' @param write_file A logical value indicating whether to write the output to a file (default is TRUE).
#' @param return_object A logical value indicating whether to return the analog data object (default is FALSE).
#' 
#' @return If `return_object` is TRUE, returns the analog data object; otherwise, returns NULL.
#' @importFrom reticulate import
#' @importFrom dplyr %>%
#' @export
get_analog_c3d_osim = function(file_input,
                               file_output = "Analog.sto",
                               write_file = TRUE,
                               return_object = FALSE){
  
  require(reticulate)
  require(dplyr)
  
  # Import OpenSim
  osim = import("opensim")
  
  # Setup adapter same as forces
  adapter = osim$C3DFileAdapter()
  adapter$setLocationForForceExpression(
    osim$C3DFileAdapter$ForceLocation_CenterOfPressure)
  
  # Get analog 
  task = adapter$read(file_input)
  analog_task = adapter$getAnalogDataTable(task)
  
  # Write to file
  if(write_file == TRUE){
    osim$STOFileAdapter_write(analog_task, file_output)
  }
  
  # Return object
  if(return_object == TRUE){
    return(analog_task) #maybe convert to df?
  }
  
}
