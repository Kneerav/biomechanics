#' Setup External Loads XML
#'
#' This function sets up an XML file for external loads by modifying
#' a given base XML file with the specified parameters (leg, body,
#' datafile) and saves the updated XML file to a specified filename.
#'
#' @param base_file A character string specifying the path to the base XML file. Default is "external_loads_base.xml".
#' @param leg A character string specifying the leg (e.g., "Left" or "Right"). Default is "Left".
#' @param body A character string specifying the body part (e.g., "calcn_l"). Default is "calcn_l".
#' @param datafile A character string specifying the name of the data file. Default is "grf.mot".
#' @param setup_filename A character string specifying the name of the output setup XML file. Default is "external_loads.xml".
#'
#' @return The function does not return a value. It writes the modified XML to the specified setup_filename.
#' @import XML
#' @export
setup_external_loads_xml = function(base_file = "external_loads_base.xml",
                                leg = "Left",
                                body = "calcn_l",
                                datafile = "grf.mot",
                                setup_filename = "external_loads.xml"){


  #Read in default file
  x = XML::xmlParse(base_file)

  #set up root
  root = XML::xmlRoot(x)

  #leg
  nodes = XML::getNodeSet(x, "//ExternalLoads//objects//ExternalForce")
  XML::addAttributes(nodes[[1]], name=leg)

  #body
  nodes = XML::getNodeSet(x, "//ExternalLoads//objects//ExternalForce//applied_to_body")
  XML::xmlValue(nodes[[1]]) = body

  #datafile
  nodes = XML::getNodeSet(x, "//ExternalLoads//datafile")
  XML::xmlValue(nodes[[1]]) = datafile

  #datasource
  nodes = XML::getNodeSet(x, "//ExternalLoads//objects//ExternalForce//data_source_name")
  XML::xmlValue(nodes[[1]]) = datafile

  #Write to file
  cat(XML::saveXML(x,
                   indent = TRUE,
                   #prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>",
                   encoding = "UTF-8"),
      file=(setup_filename))

}
