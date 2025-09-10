#' Setup Kinematic Analysis for OpenSim
#'
#' This function configures and sets up a kinematic analysis in OpenSim for a given model.
#' The analysis runs using the specified parameters, with options to save results and return
#' the analysis tool object for further use.
#'
#' @param model_file A string specifying the path to the OpenSim model file. Default is "model_scaled.osim".
#' @param replace_force_set A logical indicating whether to replace the force set. Default is FALSE.
#' @param force_set_file A string specifying the path to the force set file (e.g., "reserve_actuators.xml").
#' @param results_directory A string specifying the directory where results will be saved. Default is the current directory ("./").
#' @param start_time A numeric value indicating the start time for the analysis. Default is 0.
#' @param end_time A numeric value indicating the end time for the analysis. Default is 3.
#' @param solve_for_equilibrium_for_auxiliary_states A logical indicating whether to solve for equilibrium for auxiliary states. Default is TRUE.
#' @param coordinates_file A string specifying the path to the coordinates file. Default is NULL.
#' @param controls_file A string specifying the path to the controls file. Default is NULL.
#' @param states_file A string specifying the path to the states file. Default is NULL.
#' @param in_degrees A logical indicating whether to use degrees for coordinate values. Default is TRUE.
#' @param step_interval A numeric value specifying the step interval for the analysis. Default is 1.
#' @param setup_filename A string specifying the filename for the XML setup file. Default is "setup_ka.xml".
#' @param write_file A logical indicating whether to write the analysis tool to an XML file. Default is FALSE.
#' @param return_object A logical indicating whether to return the analysis tool object. Default is TRUE.
#'
#' @return If `return_object` is TRUE, returns the OpenSim `AnalyzeTool` object. If neither `write_file` nor `return_object` is TRUE, returns NULL invisibly.
#'
#' @import reticulate
#' @export
setup_kinematic_analysis_osim <- function(
    model_file = "model_scaled.osim",
    replace_force_set = FALSE,
    force_set_file = "reserve_actuators.xml",
    results_directory = ".",
    start_time = 0,
    end_time = 3,
    solve_for_equilibrium_for_auxiliary_states = TRUE,
    coordinates_file = NULL,
    controls_file = NULL,
    states_file = NULL,
    in_degrees = TRUE,
    step_interval = 1,
    setup_filename = "setup_ka.xml",
    write_file = FALSE,
    return_object = TRUE
) {

  #get opensim package
  osim = reticulate::import("opensim")

  #set force set string
  ForceSetFiles_string = osim$ArrayStr()
  ForceSetFiles_string$append(force_set_file)

  #setup analysis tool
  analysis_tool = osim$AnalyzeTool()
  analysis_tool$setModelFilename(model_file)
  analysis_tool$setReplaceForceSet(replace_force_set)
  analysis_tool$setForceSetFiles(ForceSetFiles_string)
  analysis_tool$setResultsDir(results_directory)
  analysis_tool$setInitialTime(start_time)
  analysis_tool$setFinalTime(end_time)
  analysis_tool$setSolveForEquilibrium(solve_for_equilibrium_for_auxiliary_states)
  analysis_tool$setCoordinatesFileName(coordinates_file)

  #set states and control if no coordinates
  if(is.null(coordinates_file)){
    analysis_tool$setControlsFileName(controls_file)
    analysis_tool$setStatesFileName(states_file)
  }

  #Kinematics analysis
  kin = osim$Kinematics()
  kin$setStartTime(start_time)
  kin$setEndTime(end_time)
  kin$setInDegrees(in_degrees)

  #add to analysis set
  Analysis_i = analysis_tool$getAnalysisSet()
  Analysis_i$cloneAndAppend(kin)

  #print to file
  if(write_file==TRUE){
    analysis_tool$printToXML(setup_filename)
  }

  #return object is requested
  if(return_object==TRUE){
    return(analysis_tool)
  }

  # Return NULL invisibly if neither write nor return is requested
  invisible(NULL)

}
