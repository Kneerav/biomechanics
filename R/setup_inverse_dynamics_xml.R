#' Set Up Inverse Dynamics XML for Simulation
#'
#' This function modifies an XML configuration file for inverse dynamics analysis. It updates the XML file with
#' various parameters such as model file, external loads, analysis times, forces to exclude, and other configuration
#' settings specific to inverse dynamics. The modified XML file is then saved to the specified setup filename.
#'
#' @param base_file Character string. The path to the base XML file to be modified (default is "setup_id_base.xml").
#' @param results_directory Character string. The directory where results should be saved (default is current directory, ".").
#' @param model_file Character string. The path to the model file to be used (default is "model_scaled.osim").
#' @param start_time Numeric. The start time of the analysis (in seconds).
#' @param end_time Numeric. The end time of the analysis (in seconds).
#' @param forces_to_exclude Character string. Forces to exclude during the analysis (default is "Muscles").
#' @param external_loads_file Character string. The path to the external loads file (default is "external_loads.xml").
#' @param coordinates_file Character string. The path to the coordinates file (default is "ik.mot").
#' @param lowpass_cutoff_frequency_for_coordinates Numeric. The cutoff frequency for lowpass filtering of coordinates (default is "8").
#' @param output_gen_force_file Character string. The name of the output file for generalized forces (default is "inverse_dynamics.sto").
#' @param setup_filename Character string. The name of the XML file to save the modified configuration (default is "setup_id.xml").
#'
#' @return This function does not return any value. It modifies the XML file in place and saves it to the provided `setup_filename`.
#'
#' @details This function uses the `XML` package to parse and modify the XML configuration file. Each parameter in the function corresponds
#' to a specific element in the XML, and its value is updated accordingly. The modified XML file is then saved to the specified location.
#'
#' @import XML
#' @export
setup_inverse_dynamics_xml <- function(base_file = "setup_id_base.xml",
                                      results_directory = ".",
                                      model_file = "model_scaled.osim",
                                      start_time,
                                      end_time,
                                      forces_to_exclude = "Muscles",
                                      external_loads_file = "external_loads.xml",
                                      coordinates_file = "ik.mot",
                                      lowpass_cutoff_frequency_for_coordinates = "8",
                                      output_gen_force_file = "inverse_dynamics.sto",
                                      setup_filename = "setup_id.xml"){


  #Read in default file
  x = XML::xmlParse(base_file)

  #set up root
  root = XML::xmlRoot(x)

  #results_directory
  nodes = XML::getNodeSet(x, "//InverseDynamicsTool//results_directory")
  XML::xmlValue(nodes[[1]]) = results_directory

  #model_file
  nodes = XML::getNodeSet(x, "//InverseDynamicsTool//model_file")
  XML::xmlValue(nodes[[1]]) = model_file

  #time_range
  time_range = paste0(start_time, " ", end_time)
  nodes = XML::getNodeSet(x, "//InverseDynamicsTool//time_range")
  XML::xmlValue(nodes[[1]]) = time_range

  #forces_to_exclude
  nodes = XML::getNodeSet(x, "//InverseDynamicsTool//forces_to_exclude")
  XML::xmlValue(nodes[[1]]) = forces_to_exclude

  #external_loads_file
  nodes = XML::getNodeSet(x, "//InverseDynamicsTool//external_loads_file")
  XML::xmlValue(nodes[[1]]) = external_loads_file

  #coordinates_file
  nodes = XML::getNodeSet(x, "//InverseDynamicsTool//coordinates_file")
  XML::xmlValue(nodes[[1]]) = coordinates_file

  #lowpass_cutoff_frequency_for_coordinates
  nodes = XML::getNodeSet(x, "//InverseDynamicsTool//lowpass_cutoff_frequency_for_coordinates")
  XML::xmlValue(nodes[[1]]) = lowpass_cutoff_frequency_for_coordinates

  #output_gen_force_file
  nodes = XML::getNodeSet(x, "//InverseDynamicsTool//output_gen_force_file")
  XML::xmlValue(nodes[[1]]) = output_gen_force_file

  #Write to file
  cat(XML::saveXML(x,
                   indent = TRUE,
                   #prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
                   encoding = "UTF-8"),
      file=(setup_filename))

}
