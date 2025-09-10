#' Setup Scale XML
#'
#' This function sets up an XML file for scaling a model by modifying a given
#' base XML file with the specified parameters (e.g., mass, height, age, etc.)
#' and saves the updated XML file to a specified filename.
#'
#' @param base_file A character string specifying the path to the base XML file. Default is "setup_scale_base.xml".
#' @param mass A numeric value specifying the mass of the model.
#' @param height A numeric value specifying the height of the model.
#' @param age A numeric value specifying the age of the model.
#' @param model_file A character string specifying the path to the baseline model file. Default is "baseline_model.osim".
#' @param marker_file A character string specifying the path to the marker data file. Default is "marker_data.trc".
#' @param coordinate_file A character string specifying the path to the coordinate file. Default is "Unassigned".
#' @param start_time A numeric value specifying the start time of the simulation.
#' @param end_time A numeric value specifying the end time of the simulation.
#' @param output_motion_file A character string specifying the output motion file name. Default is "ik.mot".
#' @param output_model_file A character string specifying the output model file name. Default is "model_scaled.osim".
#' @param output_marker_file A character string specifying the output marker file name. Default is "marker_locations.sto".
#' @param setup_filename A character string specifying the name of the output setup XML file. Default is "setup_scale.xml".
#'
#' @return The function does not return a value. It writes the modified XML to the specified setup_filename.
#' @import XML
#' @export
setup_scale_xml = function(base_file = "setup_scale_base.xml",
                           mass,
                           height,
                           age,
                           model_file = "baseline_model.osim",
                           #marker_set_file = "Markers.xml",
                           marker_file = "marker_data.trc",
                           coordinate_file = "Unassigned",
                           start_time,
                           end_time,
                           output_motion_file = "ik.mot",
                           output_model_file = "model_scaled.osim",
                           output_marker_file = "marker_locations.sto",
                           setup_filename = "setup_scale.xml"){


  #Read in default file
  x = XML::xmlParse(base_file)

  #set up root
  root = XML::xmlRoot(x)

  #mass
  nodes = XML::getNodeSet(x, "//ScaleTool//mass")
  XML::xmlValue(nodes[[1]]) = mass

  #height
  nodes = XML::getNodeSet(x, "//ScaleTool//height")
  XML::xmlValue(nodes[[1]]) = height

  #age
  nodes = XML::getNodeSet(x, "//ScaleTool//age")
  XML::xmlValue(nodes[[1]]) = age

  #model_file
  nodes = XML::getNodeSet(x, "//ScaleTool//GenericModelMaker//model_file")
  XML::xmlValue(nodes[[1]]) = model_file

  #marker_set_file
  #nodes = XML::getNodeSet(x, "//ScaleTool//GenericModelMaker//marker_set_file")
  #XML::xmlValue(nodes[[1]]) = marker_set_file

  #marker_file
  nodes = XML::getNodeSet(x, "//ScaleTool//MarkerPlacer//marker_file")  #markerplacer
  XML::xmlValue(nodes[[1]]) = marker_file

  nodes = XML::getNodeSet(x, "//ScaleTool//ModelScaler//marker_file")  #modelscaler
  XML::xmlValue(nodes[[1]]) = marker_file

  #coordinate_file
  nodes = XML::getNodeSet(x, "//ScaleTool//coordinate_file")
  XML::xmlValue(nodes[[1]]) = coordinate_file

  #time_range
  time_range = paste0(start_time, " ", end_time)
  nodes = XML::getNodeSet(x, "//ScaleTool//MarkerPlacer//time_range") #markerplacer
  XML::xmlValue(nodes[[1]]) = time_range

  nodes = XML::getNodeSet(x, "//ScaleTool//ModelScaler//time_range") #modelscaler
  XML::xmlValue(nodes[[1]]) = time_range

  #output_motion_file
  nodes = XML::getNodeSet(x, "//ScaleTool//MarkerPlacer//output_motion_file")
  XML::xmlValue(nodes[[1]]) = output_motion_file

  #output_model_file
  nodes = XML::getNodeSet(x, "//ScaleTool//MarkerPlacer//output_model_file") #markerplacer
  XML::xmlValue(nodes[[1]]) = output_model_file

  nodes = XML::getNodeSet(x, "//ScaleTool//ModelScaler//output_model_file") #modelscaler
  XML::xmlValue(nodes[[1]]) = output_model_file

  #output_marker_file
  nodes = XML::getNodeSet(x, "//ScaleTool//MarkerPlacer//output_marker_file")
  XML::xmlValue(nodes[[1]]) = output_marker_file

  #Write to file
  cat(XML::saveXML(x,
                   indent = TRUE,
                   #prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
                   encoding = "UTF-8"),
      file=(setup_filename))

}
