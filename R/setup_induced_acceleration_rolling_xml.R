#' Set Up Induced Acceleration Rolling XML for Simulation
#'
#' This function modifies an XML configuration file for induced acceleration analysis with rolling constraints.
#' It updates the XML file with information such as the model file, force set files, results directory,
#' analysis times, and specific rolling constraint parameters. The modified XML file is saved to the specified
#' setup filename.
#'
#' @param base_file Character string. The path to the base XML file to be modified (default is "setup_iaa_base.xml").
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
#' @param step_interval Numeric. The step interval for the induced acceleration analysis (default is "1").
#' @param in_degrees Character string. Whether the results should be expressed in degrees ("true" or "false", default is "true").
#' @param coordinate_names Character string. The list of coordinates to include in the analysis (default is "all").
#' @param body_names Character string. The list of bodies to include in the analysis (default is "all").
#' @param constraint_type Character string. The type of constraint ("roll", "pitch", etc., default is "roll").
#' @param force_threshold Numeric. The threshold force for inclusion in the analysis (default is 5).
#' @param compute_potentials_only Character string. Whether to compute only potentials ("true" or "false", default is "true").
#' @param report_constraint_reactions Character string. Whether to report constraint reactions ("true" or "false", default is "true").
#' @param rolling_body Character string. The name of the rolling body (default is "calcn_r").
#' @param surface_body Character string. The name of the surface body (default is "ground").
#' @param surface_normal Character string. The normal vector of the surface (default is "0 1 0").
#' @param surface_height Numeric. The height of the surface (default is 0).
#' @param friction_coefficient Numeric. The coefficient of friction for the surface (default is 0.65).
#' @param contact_radius Numeric. The radius of contact (default is 0.01).
#' @param external_loads_file Character string. The path to the external loads file (default is "external_loads.xml").
#' @param coordinates_file Character string. The path to the coordinates file (default is "ik.mot").
#' @param lowpass_cutoff_frequency_for_coordinates Numeric. The cutoff frequency for lowpass filtering of coordinates (default is "12").
#' @param setup_filename Character string. The name of the XML file to save the modified configuration (default is "setup_iaa.xml").
#'
#' @return This function does not return any value. It modifies the XML file in place and saves it to the provided `setup_filename`.
#'
#' @details This function uses the `XML` package to parse and modify the XML configuration file. Each parameter in the function corresponds
#' to a specific element in the XML, and its value is updated accordingly. The modified XML file is then saved to the specified location.
#'
#' @import XML
#' @export
setup_induced_acceleration_rolling_xml <- function(base_file = "setup_iaa_base.xml",
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
                                          coordinate_names = "all",
                                          body_names = "all",
                                          constraint_type = "roll",
                                          force_threshold = "5",
                                          compute_potentials_only = "true",
                                          report_constraint_reactions = "true",
                                          rolling_body = "calcn_r",
                                          surface_body = "ground",
                                          surface_normal = "0 1 0",
                                          surface_height = "0",
                                          friction_coefficient = "0.65",
                                          contact_radius = "0.01",
                                          external_loads_file = "external_loads.xml",
                                          coordinates_file = "ik.mot",
                                          lowpass_cutoff_frequency_for_coordinates = "12",
                                          setup_filename = "setup_iaa.xml"){


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

  ###################   Analysis set, specifics to IAA    ###############################
  #start_time
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//start_time")
  XML::xmlValue(nodes[[1]]) = start_time

  #end_time
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//end_time")
  XML::xmlValue(nodes[[1]]) = end_time

  #step_interval
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//step_interval")
  XML::xmlValue(nodes[[1]]) = step_interval

  #in_degrees
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//in_degrees")
  XML::xmlValue(nodes[[1]]) = in_degrees

  #coordinate_names
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//coordinate_names")
  XML::xmlValue(nodes[[1]]) = coordinate_names

  #body_names
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//body_names")
  XML::xmlValue(nodes[[1]]) = body_names

  #constraint_type
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//constraint_type")
  XML::xmlValue(nodes[[1]]) = constraint_type

  #force_threshold
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//force_threshold")
  XML::xmlValue(nodes[[1]]) = force_threshold

  #compute_potentials_only
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//compute_potentials_only")
  XML::xmlValue(nodes[[1]]) = compute_potentials_only

  #report_constraint_reactions
  nodes = XML::getNodeSet(x, "//AnalysisSet//objects//InducedAccelerations//report_constraint_reactions")
  XML::xmlValue(nodes[[1]]) = report_constraint_reactions

  ########################## CONSTRAINT SET INFO #####################################################

  #rolling_body
  nodes = XML::getNodeSet(x, "//ConstraintSet//objects//RollingOnSurfaceConstraint//rolling_body")
  XML::xmlValue(nodes[[1]]) = rolling_body

  #surface_body
  nodes = XML::getNodeSet(x, "//ConstraintSet//objects//RollingOnSurfaceConstraint//surface_body")
  XML::xmlValue(nodes[[1]]) = surface_body

  #surface_normal
  nodes = XML::getNodeSet(x, "//ConstraintSet//objects//RollingOnSurfaceConstraint//surface_normal")
  XML::xmlValue(nodes[[1]]) = surface_normal

  #surface_height
  nodes = XML::getNodeSet(x, "//ConstraintSet//objects//RollingOnSurfaceConstraint//surface_height")
  XML::xmlValue(nodes[[1]]) = surface_height

  #friction_coefficient
  nodes = XML::getNodeSet(x, "//ConstraintSet//objects//RollingOnSurfaceConstraint//friction_coefficient")
  XML::xmlValue(nodes[[1]]) = friction_coefficient

  #contact_radius
  nodes = XML::getNodeSet(x, "//ConstraintSet//objects//RollingOnSurfaceConstraint//contact_radius")
  XML::xmlValue(nodes[[1]]) = contact_radius

  #Write to file
  cat(XML::saveXML(x,
                   indent = TRUE,
                   #prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
                   encoding = "UTF-8"),
      file=(setup_filename))

}
