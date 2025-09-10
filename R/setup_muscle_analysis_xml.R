#' Set Up Muscle Analysis XML for Simulation
#'
#' This function modifies an XML configuration file for muscle analysis by updating
#' various parameters related to the analysis setup. It updates the XML file with
#' information such as the model file, force set files, results directory, analysis
#' times, and other solver options. The modified XML file is saved to the specified
#' setup filename.
#'
#' @param base_file Character string. The path to the base XML file to be modified (default is "setup_ma_base.xml").
#' @param model_file Character string. The path to the model file to be used (default is "model_scaled.osim").
#' @param replace_force_set Character string. Whether to replace the force set ("true" or "false", default is "false").
#' @param force_set_files Character string. The path to the force set files to be used (default is "reserve_actuators.xml").
#' @param results_directory Character string. The directory where results should be saved (default is current directory, ".").
#' @param output_precision Character string. The desired precision for output (default is "12").
#' @param start_time Numeric. The start time of the analysis (in seconds).
#' @param end_time Numeric. The end time of the analysis (in seconds).
#' @param solve_for_equilibrium_for_auxiliary_states Character string. Whether to solve for equilibrium for auxiliary states ("true" or "false", default is "false").
#' @param maximum_number_of_integrator_steps Numeric. The maximum number of integrator steps (default is 20000).
#' @param maximum_integrator_step_size Numeric. The maximum size of each integrator step (default is 1).
#' @param minimum_integrator_step_size Numeric. The minimum size of each integrator step (default is 1e-05).
#' @param integrator_error_tolerance Numeric. The error tolerance for the integrator (default is 1e-05).
#' @param step_interval Numeric. The step interval for the muscle analysis (default is "1").
#' @param in_degrees Character string. Whether the results should be expressed in degrees ("true" or "false", default is "true").
#' @param muscle_list Character string. The list of muscles to include in the analysis (default is "all").
#' @param moment_arm_coordinate_list Character string. The list of coordinates for moment arm calculations (default is "all").
#' @param compute_moments Character string. Whether to compute muscle moments ("true" or "false", default is "true").
#' @param external_loads_file Character string. The path to the external loads file (default is "external_loads.xml").
#' @param coordinates_file Character string. The path to the coordinates file (default is "ik.mot").
#' @param lowpass_cutoff_frequency_for_coordinates Numeric. The cutoff frequency for lowpass filtering of coordinates (default is "12").
#' @param setup_filename Character string. The name of the XML file to save the modified configuration (default is "setup_ma.xml").
#'
#' @return This function does not return any value. It modifies the XML file in place and saves it to the provided `setup_filename`.
#'
#' @details This function uses the `XML` package to parse and modify the XML configuration file. Each parameter in the function corresponds
#' to a specific element in the XML, and its value is updated accordingly. The modified XML file is then saved to the specified location.
#'
#' @import XML
#' @export
setup_muscle_analysis_xml <- function(base_file = "setup_ma_base.xml",
                    model_file = "model_scaled.osim",
                    replace_force_set = "false",
                    force_set_files = "reserve_actuators.xml",
                    results_directory = ".",
                    output_precision = "12",
                    start_time,
                    end_time,
                    solve_for_equilibrium_for_auxiliary_states = "false",
                    maximum_number_of_integrator_steps = "20000",
                    maximum_integrator_step_size = "1",
                    minimum_integrator_step_size = "1e-05",
                    integrator_error_tolerance = "1e-05",
                    step_interval = "1",
                    in_degrees = "true",
                    muscle_list = "all",
                    moment_arm_coordinate_list = "all",
                    compute_moments = "true",
                    external_loads_file = "external_loads.xml",
                    coordinates_file = "ik.mot",
                    lowpass_cutoff_frequency_for_coordinates = "12",
                    setup_filename = "setup_ma.xml"){


  #Read in default file
  x = XML::xmlParse(base_file)

  #set up root
  root = XML::xmlRoot(x)

  #model_file
  nodes = XML::getNodeSet(x, "//AnalyzeTool//model_file")
  XML::xmlValue(nodes[[1]]) = model_file

  #replace_force_set
  nodes = XML::getNodeSet(x, "//AnalyzeTool//replace_force_set")
  XML::xmlValue(nodes[[1]]) = replace_force_set

  #force_set_files
  nodes = XML::getNodeSet(x, "//AnalyzeTool//force_set_files")
  XML::xmlValue(nodes[[1]]) = force_set_files

  #results_directory
  nodes = XML::getNodeSet(x, "//AnalyzeTool//results_directory")
  XML::xmlValue(nodes[[1]]) = results_directory

  #output_precision
  nodes = XML::getNodeSet(x, "//AnalyzeTool//output_precision")
  XML::xmlValue(nodes[[1]]) = output_precision

  #initial_time
  nodes = XML::getNodeSet(x, "//AnalyzeTool//initial_time")
  XML::xmlValue(nodes[[1]]) = start_time

  #final_time
  nodes = XML::getNodeSet(x, "//AnalyzeTool//final_time")
  XML::xmlValue(nodes[[1]]) = end_time

  #solve_for_equilibrium_for_auxiliary_states
  nodes = XML::getNodeSet(x, "//AnalyzeTool//solve_for_equilibrium_for_auxiliary_states")
  XML::xmlValue(nodes[[1]]) = solve_for_equilibrium_for_auxiliary_states

  #maximum_number_of_integrator_steps
  nodes = XML::getNodeSet(x, "//AnalyzeTool//maximum_number_of_integrator_steps")
  XML::xmlValue(nodes[[1]]) = maximum_number_of_integrator_steps

  #maximum_integrator_step_size
  nodes = XML::getNodeSet(x, "//AnalyzeTool//maximum_integrator_step_size")
  XML::xmlValue(nodes[[1]]) = maximum_integrator_step_size


  #minimum_integrator_step_size
  nodes = XML::getNodeSet(x, "//AnalyzeTool//minimum_integrator_step_size")
  XML::xmlValue(nodes[[1]]) = minimum_integrator_step_size

  #integrator_error_tolerance
  nodes = XML::getNodeSet(x, "//AnalyzeTool//integrator_error_tolerance")
  XML::xmlValue(nodes[[1]]) = integrator_error_tolerance

  #external_loads_file
  nodes = XML::getNodeSet(x, "//AnalyzeTool//external_loads_file")
  XML::xmlValue(nodes[[1]]) = external_loads_file

  #coordinates_file
  nodes = XML::getNodeSet(x, "//AnalyzeTool//coordinates_file")
  XML::xmlValue(nodes[[1]]) = coordinates_file

  #lowpass_cutoff_frequency_for_coordinates
  nodes = XML::getNodeSet(x, "//AnalyzeTool//lowpass_cutoff_frequency_for_coordinates")
  XML::xmlValue(nodes[[1]]) = lowpass_cutoff_frequency_for_coordinates

  ###################   Analysis set, specifics to MA    ###############################
  #start_time
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//MuscleAnalysis//start_time")
  XML::xmlValue(nodes[[1]]) = start_time

  #end_time
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//MuscleAnalysis//end_time")
  XML::xmlValue(nodes[[1]]) = end_time

  #step_interval
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//MuscleAnalysis//step_interval")
  XML::xmlValue(nodes[[1]]) = step_interval

  #in_degrees
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//MuscleAnalysis//in_degrees")
  XML::xmlValue(nodes[[1]]) = in_degrees

  #muscle_list
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//MuscleAnalysis//muscle_list")
  XML::xmlValue(nodes[[1]]) = muscle_list

  #moment_arm_coordinate_list
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//MuscleAnalysis//moment_arm_coordinate_list")
  XML::xmlValue(nodes[[1]]) = moment_arm_coordinate_list

  #compute_moments
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//MuscleAnalysis//compute_moments")
  XML::xmlValue(nodes[[1]]) = compute_moments

  #Write to file
  cat(XML::saveXML(x,
                   indent = TRUE,
                   #prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
                   encoding = "UTF-8"),
      file=(setup_filename))

}
