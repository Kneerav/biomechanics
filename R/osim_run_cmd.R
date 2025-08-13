#' Run OpenSim Command Line Tool
#'
#' This function executes an OpenSim command line tool using `opensim-cmd`.
#'
#' @param opensim_path A character string specifying the path to the OpenSim bin directory.
#' @param setup_file A character string specifying the path to the setup file (.xml) to be run with OpenSim.
#'
#' @return The result of running the OpenSim command.
#' @export
osim_run_cmd = function(opensim_path = 'C:/"OpenSim 4.5"/bin',
                        setup_file){

  # Check if the setup file exists
  if (!file.exists(setup_file)) {
    stop("The specified setup file does not exist.")
  }

  #Rewrite command to fix
  run_tool = paste0(opensim_path, "/opensim-cmd run-tool ")
  setup_file = paste0('"', setup_file, '"')

  #setup command
  CMD = paste0(run_tool, setup_file)

  #run command through system
  cat(system(CMD, intern = FALSE, wait=TRUE, show.output.on.console = FALSE))

}
