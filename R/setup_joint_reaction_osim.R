#' Setup Joint Reaction for OpenSim Analysis
#'
#' This function sets up an OpenSim `AnalyzeTool` for calculating joint reactions
#' and generates the corresponding XML setup file. The function configures parameters
#' for the model, force sets, and joint reactions, and can either save the setup to
#' a file or return the configured `AnalyzeTool` object for further use.
#'
#' @param model_file A character string specifying the path to the scaled OpenSim model file. Default is "model_scaled.osim".
#' @param replace_force_set A logical value indicating whether to replace the force set. Default is `FALSE`.
#' @param force_set_file A character string specifying the path to the force set XML file. Default is "reserve_actuators.xml".
#' @param results_directory A character string specifying the directory for storing results. Default is the current working directory.
#' @param start_time A numeric value specifying the start time for the analysis. Default is `0`.
#' @param end_time A numeric value specifying the end time for the analysis. Default is `3`.
#' @param solve_for_equilibrium_for_auxiliary_states A logical value specifying whether to solve for equilibrium for auxiliary states. Default is `TRUE`.
#' @param coordinates_file A character string specifying the path to the coordinates file. Default is `NULL`.
#' @param controls_file A character string specifying the path to the controls file. Default is `NULL`.
#' @param states_file A character string specifying the path to the states file. Default is `NULL`.
#' @param in_degrees A logical value indicating whether the joint reaction angles should be returned in degrees. Default is `TRUE`.
#' @param forces_file A character string specifying the path to the forces file. Default is "force.sto".
#' @param on_body A character string specifying the body to which the joint reaction is applied. Default is 'child'.
#' @param in_frame A character string specifying the frame in which to compute the joint reaction. Default is "child".
#' @param joint_name A character string specifying the name of the joint(s) for the joint reaction. Default is 'all'.
#' @param step_interval A numeric value specifying the step interval for the analysis. Default is `1`.
#' @param setup_filename A character string specifying the name of the output setup XML file. Default is "setup_jr.xml".
#' @param write_file A logical value indicating whether to write the XML setup file to disk. Default is `FALSE`.
#' @param return_object A logical value indicating whether to return the `AnalyzeTool` object. Default is `TRUE`.
#'
#' @return If `write_file` is `TRUE`, writes the XML setup file to disk. If `return_object` is `TRUE`, returns the `AnalyzeTool` object.
#'         If neither is `TRUE`, the function returns `NULL` invisibly.
#' @import reticulate
#' @export
setup_joint_reaction_osim <- function(
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
    forces_file = "force.sto",
    on_body = 'parent',
    in_frame = "child",
    joint_name = 'all',
    step_interval = 1,
    setup_filename = "setup_jr.xml",
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

  #set jr strings (switch to loop lists)
  InFrame_string = osim$ArrayStr()
  InFrame_string$append(in_frame)

  OnBody_string = osim$ArrayStr()
  OnBody_string$append(on_body)

  JointNames_string = osim$ArrayStr()
  JointNames_string$append(joint_name)

  #create joint reaction
  jr = osim$JointReaction()
  jr$setName("JointReaction")
  jr$setStartTime(start_time)
  jr$setEndTime(end_time)
  jr$setInDegrees(in_degrees)
  jr$setStepInterval(step_interval)
  jr$setJointNames(osim$ArrayStr(JointNames_string))
  jr$setInFrame(osim$ArrayStr(InFrame_string))
  jr$setOnBody(osim$ArrayStr(OnBody_string))
  jr$setForcesFileName(forces_file)

  #add to analysis set
  Analysis_i = analysis_tool$getAnalysisSet()
  Analysis_i$cloneAndAppend(jr)

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
