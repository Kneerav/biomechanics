#' Visualize Marker Data in OpenSim
#'
#' This function visualizes marker data in OpenSim using the `opensim-cmd` tool.
#'
#' @param opensim_path A character string specifying the path to the OpenSim bin directory.
#' @param trc_file_path A character string specifying the path to the TRC file containing the marker data (.trc).
#'
#' @return The result of running the OpenSim command.
#' @export
osim_viz_trc_cmd = function(opensim_path = 'C:/"OpenSim 4.5"/bin',
                            trc_file_path = "marker.trc"){

  # Check if the TRC file exists
  if (!file.exists(trc_file_path)) {
    stop("The specified TRC file does not exist.")
  }

  #Rewrite command to fix
  run_tool = paste0(opensim_path, "/opensim-cmd viz data ")
  trc_file = paste0('"', trc_file_path, '"', " ")

  #setup command
  CMD = paste0(run_tool, trc_file)

  #run command through system
  cat(system(CMD, intern = FALSE, wait=FALSE, show.output.on.console = TRUE))

}
