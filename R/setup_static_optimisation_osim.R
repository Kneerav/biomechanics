#' Setup an OpenSim Static Optimization Analysis
#'
#' This function sets up an OpenSim Static Optimization analysis using the
#' OpenSim Python API (via `reticulate`). It creates and configures an
#' `AnalyzeTool` object with a `StaticOptimization` analysis and optionally
#' writes the setup file to XML or returns the configured tool object.
#'
#' @param model_file Character. Path to the scaled OpenSim model file (default: `"model_scaled.osim"`).
#' @param replace_force_set Logical. Whether to replace the model's existing force set (default: `FALSE`).
#' @param force_set_file Character. Path to the force set XML file (default: `"reserve_actuators.xml"`).
#' @param results_directory Character. Directory where results will be written (default: `"."`).
#' @param output_precision Integer. Output precision for numerical results (default: `12L`).
#' @param start_time Numeric. Start time for the analysis (default: `0`).
#' @param end_time Numeric. End time for the analysis (default: `3`).
#' @param solve_for_equilibrium_for_auxiliary_states Logical. Whether to solve for equilibrium of auxiliary states (default: `FALSE`).
#' @param maximum_number_of_integrator_steps Integer. Maximum number of integrator steps (default: `20000L`).
#' @param maximum_integrator_step_size Numeric. Maximum integrator step size (default: `1`).
#' @param minimum_integrator_step_size Numeric. Minimum integrator step size (default: `1e-8`).
#' @param integrator_error_tolerance Numeric. Integrator error tolerance (default: `1e-5`).
#' @param coordinates_file Character or `NULL`. Path to a motion (coordinates) file (default: `NULL`).
#' @param use_model_force_set Logical. Whether to use the model's own force set in static optimization (default: `TRUE`).
#' @param activation_exponent Numeric. Activation exponent for optimization (default: `2`).
#' @param use_muscle_physiology Logical. Whether to use muscle physiology in optimization (default: `TRUE`).
#' @param in_degrees Logical. Whether coordinate data is in degrees (default: `TRUE`).
#' @param external_loads_file Character. Path to external loads file (default: `"external_loads.xml"`).
#' @param step_interval Integer. Step interval for optimization (default: `1L`).
#' @param lowpass_cutoff_frequency_for_coordinates Numeric. Low-pass filter cutoff frequency for coordinate data (required).
#' @param setup_filename Character. Name of the XML setup file to write (default: `"setup_so.xml"`).
#' @param write_file Logical. Whether to write the setup XML file (default: `FALSE`).
#' @param return_object Logical. Whether to return the configured `AnalyzeTool` object (default: `TRUE`).
#'
#' @return If `return_object = TRUE`, returns an OpenSim `AnalyzeTool` object. Otherwise, invisibly returns `NULL`.
#'
#' @details
#' This helper function is designed to simplify the setup of Static Optimization analyses
#' in OpenSim. It builds and configures the `AnalyzeTool`, optionally adds the
#' `StaticOptimization` analysis, and can write the configuration to XML for use
#' in OpenSim GUI or scripts.
#'
#' @import reticulate
#'
#' @export
setup_static_optimisation_osim <- function(
    model_file = "model_scaled.osim",
    replace_force_set = FALSE,
    force_set_file = "reserve_actuators.xml",
    results_directory = ".",
    output_precision = 12L,
    start_time = 0,
    end_time = 3,
    solve_for_equilibrium_for_auxiliary_states = FALSE,
    maximum_number_of_integrator_steps = 20000L,
    maximum_integrator_step_size = 1,
    minimum_integrator_step_size = 1e-08,
    integrator_error_tolerance = 1e-05,
    coordinates_file = NULL,
    use_model_force_set = TRUE,
    activation_exponent = 2,
    use_muscle_physiology = TRUE,
    in_degrees = TRUE,
    external_loads_file = "external_loads.xml",
    step_interval = 1L,
    lowpass_cutoff_frequency_for_coordinates,
    setup_filename = "setup_so.xml",
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

  #basics
  analysis_tool$setModelFilename(model_file)
  analysis_tool$setReplaceForceSet(replace_force_set)
  analysis_tool$setForceSetFiles(ForceSetFiles_string)
  analysis_tool$setResultsDir(results_directory)
  analysis_tool$setInitialTime(start_time)
  analysis_tool$setFinalTime(end_time)
  analysis_tool$setSolveForEquilibrium(solve_for_equilibrium_for_auxiliary_states)

  #motion files
  analysis_tool$setCoordinatesFileName(coordinates_file)
  analysis_tool$setExternalLoadsFileName(external_loads_file)
  analysis_tool$setLowpassCutoffFrequency(lowpass_cutoff_frequency_for_coordinates)

  #solver stuff
  analysis_tool$setOutputPrecision(output_precision)
  analysis_tool$setMinDT(minimum_integrator_step_size)
  analysis_tool$setMaxDT(maximum_integrator_step_size)
  analysis_tool$setErrorTolerance(integrator_error_tolerance)
  analysis_tool$setMaximumNumberOfSteps(maximum_number_of_integrator_steps)

  #so
  so = osim$StaticOptimization()
  so$setAllPropertiesUseDefault(TRUE)
  so$setStartTime(start_time)
  so$setEndTime(end_time)
  so$setActivationExponent(activation_exponent)
  so$setStepInterval(step_interval)
  so$setUseMusclePhysiology(use_muscle_physiology)
  so$setUseModelForceSet(use_model_force_set)
  so$setInDegrees(in_degrees)

  #solver stuff
  #so$setConvergenceCriterion(integrator_error_tolerance)
  #so$setMaxIterations(maximum_number_of_integrator_steps)

  #add to analysis set
  Analysis_i = analysis_tool$getAnalysisSet()
  Analysis_i$cloneAndAppend(so)

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
