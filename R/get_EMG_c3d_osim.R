#' Get EMG Data from C3D File
#'
#' This function reads a C3D file, extracts EMG data, and optionally writes it to a .sto file
#' and/or returns the data as a data frame.
#'
#' @param file_input A string specifying the path to the input C3D file.
#' @param file_output A string specifying the path for the output .sto file (default is "EMG_RAW.sto").
#' @param columns_exclude A numeric vector of indices for columns to exclude from the EMG data.
#' @param EMG_names A character vector specifying the names of the EMG channels to extract (default includes "Voltage.1" to "Voltage.16").
#' @param Muscle_names A character vector specifying the muscle names corresponding to the EMG data (default provided).
#' @param write_file A logical value indicating whether to write the output to a file (default is TRUE).
#' @param return_object A logical value indicating whether to return the EMG data as a data frame (default is FALSE).
#'
#' @return If `return_object` is TRUE, returns the EMG data as a data frame; otherwise, returns NULL.
#' @importFrom reticulate import
#' @importFrom dplyr %>%
#' @export
get_EMG_c3d_osim = function(file_input,
                            file_output = "EMG_RAW.sto",
                            columns_exclude = c(1L:40L),
                            EMG_names = c("Voltage.1", "Voltage.2", "Voltage.3","Voltage.4",
                                          "Voltage.6", "Voltage.7", "Voltage.8",
                                          "Voltage.9", "Voltage.10", "Voltage.12",
                                          "Voltage.13", "Voltage.14", "Voltage.15", "Voltage.16"),
                            Muscle_names = c("vaslat_r", "recfem_r", "vasmed_r","vasmed_l",
                                             "vaslat_l", "tfl_r", "tfl_l",
                                             "bflh_l", "semiten_l", "semiten_r", "bflh_r",
                                             "gaslat_l", "gasmed_r", "gaslat_r"),
                            write_file = TRUE,
                            return_object = FALSE){

  require(reticulate)
  require(dplyr)

  # Import OpenSim
  osim = import("opensim")

  # Setup adapter same as forces
  adapter = osim$C3DFileAdapter()
  adapter$setLocationForForceExpression(osim$C3DFileAdapter$ForceLocation_CenterOfPressure)

  # Get analog data
  task = adapter$read(file_input)
  analog_task = adapter$getAnalogDataTable(task)

  # Remove extra columns
  for(col_i in rev(columns_exclude - 1L)){
    analog_task$removeColumnAtIndex(col_i)
  }

  # Rename columns
  time = analog_task$getIndependentColumn() %>% unlist()
  EMG_data = matrix(ncol = length(EMG_names), nrow = length(time))
  for(emg_i in 1L:length(EMG_names)){
    emg_col_i = analog_task$getDependentColumn(EMG_names[emg_i])$to_numpy()
    EMG_data[,emg_i] = emg_col_i
  }

  # Create data frame
  EMG_data_df = as.data.frame(cbind(time, EMG_data)) %>%
    `colnames<-`(c("time", Muscle_names))

  # Write to file
  if(write_file == TRUE){
    write_mot_sto(EMG_data_df, "EMG", "no", file_output)
  }

  # Return object
  if(return_object == TRUE){
    return(EMG_data_df)
  }

}
