#' Visualize OpenSim Model
#'
#' This function visualizes an OpenSim model using the `opensim-cmd` tool.
#'
#' @param opensim_path A character string specifying the path to the OpenSim bin directory.
#' @param model_file_path A character string specifying the path to the model file (.osim).
#'
#' @return The result of running the OpenSim command.
#' @export
osim_viz_model_cmd = function(opensim_path = 'C:/"OpenSim 4.5"/bin',
                              model_file_path = "Trunk_pelvis_LL.osim"){

  # Check if the model file exists
  if (!file.exists(model_file_path)) {
    stop("The specified model file does not exist.")
  }

  #Rewrite command to fix
  run_tool = paste0(opensim_path, "/opensim-cmd viz model ")
  model_file = paste0('"', model_file_path, '"', " ")

  #setup command
  CMD = paste0(run_tool, model_file)

  #run command through system
  cat(system(CMD, intern = FALSE, wait=FALSE, show.output.on.console = TRUE))

}
