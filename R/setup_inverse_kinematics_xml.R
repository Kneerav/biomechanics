#' Setup Inverse Kinematics XML
#'
#' This function sets up an XML file for inverse kinematics by modifying
#' a given base XML file with the specified parameters (e.g., model, marker
#' data, time range, etc.) and saves the updated XML file to a specified filename.
#'
#' @param base_file A character string specifying the path to the base XML file. Default is "setup_ik_base.xml".
#' @param results_directory A character string specifying the directory for saving results. Default is the current directory (".").
#' @param model_file A character string specifying the path to the scaled model file. Default is "model_scaled.osim".
#' @param constraint_weight A character string or numeric specifying the constraint weight. Default is "Inf".
#' @param accuracy A character string or numeric specifying the desired accuracy. Default is "1e-005".
#' @param marker_file A character string specifying the path to the marker data file. Default is "marker_data.trc".
#' @param coordinate_file A character string specifying the path to the coordinate file. Default is "Unassigned".
#' @param start_time A numeric value specifying the start time of the simulation.
#' @param end_time A numeric value specifying the end time of the simulation.
#' @param report_errors A character string specifying whether to report errors ("true" or "false"). Default is "true".
#' @param output_motion_file A character string specifying the output motion file name. Default is "ik.mot".
#' @param report_marker_locations A character string specifying whether to report marker locations ("true" or "false"). Default is "true".
#' @param Markers A character vector specifying the names of the markers used in the inverse kinematics setup.
#' @param Weights A numeric vector specifying the weight for each marker. Default is a vector of 1's with the same length as `Markers`.
#' @param setup_filename A character string specifying the name of the output setup XML file. Default is "setup_ik.xml".
#'
#' @return The function does not return a value. It writes the modified XML to the specified setup_filename.
#' @import XML
#' @export
setup_inverse_kinematics_xml = function(base_file = "setup_ik_base.xml",
                                        results_directory = ".",
                                        model_file = "model_scaled.osim",
                                        constraint_weight = "Inf",
                                        accuracy = "1e-005",
                                        marker_file = "marker_data.trc",
                                        coordinate_file = "Unassigned",
                                        start_time,
                                        end_time,
                                        report_errors = "true",
                                        output_motion_file = "ik.mot",
                                        report_marker_locations = "true",
                                        Markers,
                                        Weights = rep(1, length(Markers)),
                                        setup_filename = "setup_ik.xml"){


  #Read in default file
  x = XML::xmlParse(base_file)

  #set up root
  root = XML::xmlRoot(x)

  #results_directory
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//results_directory")
  XML::xmlValue(nodes[[1]]) = results_directory

  #model_file
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//model_file")
  XML::xmlValue(nodes[[1]]) = model_file

  #constraint_weight
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//constraint_weight")
  XML::xmlValue(nodes[[1]]) = constraint_weight

  #accuracy
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//accuracy")
  XML::xmlValue(nodes[[1]]) = accuracy

  #marker_file
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//marker_file")
  XML::xmlValue(nodes[[1]]) = marker_file

  #coordinate_file
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//coordinate_file")
  XML::xmlValue(nodes[[1]]) = coordinate_file

  #time_range
  time_range = paste0(start_time, " ", end_time)
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//time_range")
  XML::xmlValue(nodes[[1]]) = time_range

  #report_errors
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//report_errors")
  XML::xmlValue(nodes[[1]]) = report_errors

  #output_motion_file
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//output_motion_file")
  XML::xmlValue(nodes[[1]]) = output_motion_file

  #report_marker_locations
  nodes = XML::getNodeSet(x, "//InverseKinematicsTool//report_marker_locations")
  XML::xmlValue(nodes[[1]]) = report_marker_locations

  #Write to file
  cat(XML::saveXML(x,
                   indent = TRUE,
                   #prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
                   encoding = "UTF-8"),
      file=(setup_filename))

}
