#' Setup Muscle Analysis for OpenSim
#'
#' This function configures and sets up a muscle analysis in OpenSim for a given model.
#' The analysis will run using the specified parameters, with options to save results,
#' and return the analysis tool object for further use.
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
#' @param muscle_list A character vector specifying the list of muscles to include in the analysis. Default is `c("bflh_r", "bfsh_r", "semimem_r", "semiten_r")`.
#' @param moment_arm_coordinate_list A character vector specifying the coordinates for moment arm calculation. Default is `c("knee_angle_r", "hip_flexion_r")`.
#' @param compute_moments A logical indicating whether to compute muscle moment arms. Default is TRUE.
#' @param step_interval A numeric value specifying the step interval for the analysis. Default is 1.
#' @param setup_filename A string specifying the filename for the XML setup file. Default is "setup_ma.xml".
#' @param write_file A logical indicating whether to write the analysis tool to an XML file. Default is FALSE.
#' @param return_object A logical indicating whether to return the analysis tool object. Default is TRUE.
#'
#' @return If `return_object` is TRUE, returns the OpenSim `AnalyzeTool` object. If neither `write_file` nor `return_object` is TRUE, returns NULL invisibly.
#'
#' @import reticulate
#' @export
setup_muscle_analysis_osim <- function(
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
    muscle_list = c("bflh_r", "bfsh_r", "semimem_r", "semiten_r"),
    moment_arm_coordinate_list = c("knee_angle_r", "hip_flexion_r"),
    compute_moments = TRUE,
    step_interval = 1,
    setup_filename = "setup_ma.xml",
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

  #Muscle analysis
  ma = osim$MuscleAnalysis()
  ma$setStartTime(start_time)
  ma$setEndTime(end_time)
  ma$setComputeMoments(compute_moments)

  ##set muscles
  muscles = osim$ArrayStr()
  for(k in 1:length(muscle_list)){
    muscles$append(muscle_list[[k]])
  }
  ma$setMuscles(muscles)

  ##set coordinates for moment arms
  coordinates = osim$ArrayStr()
  for(k in 1:length(moment_arm_coordinate_list)){
    coordinates$append(moment_arm_coordinate_list[[k]])
  }
  ma$setCoordinates(coordinates)

  #add to analysis set
  Analysis_i = analysis_tool$getAnalysisSet()
  Analysis_i$cloneAndAppend(ma)

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
