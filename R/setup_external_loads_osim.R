#' Create an OpenSim External Loads XML File
#'
#' This function creates an OpenSim External Loads XML setup file
#' for applying external forces (e.g., ground reaction forces) to a model.
#' It is useful for preparing input files for inverse dynamics or forward simulations.
#'
#' @param setup_filename Character. Name of the XML file to write (default: `"external_loads.xml"`).
#' @param set_name Character vector. Names of the external load elements (e.g., `"right"`, `"left"`).
#' @param set_applied_body_name Character vector. Names of the bodies to which each load is applied (e.g., `"calcn_r"`).
#' @param force_expressed_in_body_name Character. Body in which forces are expressed.
#' @param point_expressed_in_body_name Character. Body in which point locations are expressed.
#' @param set_force_identifier Character vector. Identifiers for force data columns in the `.mot` file (e.g., `"fx_"`).
#' @param set_point_identifier Character vector. Identifiers for point location columns in the `.mot` file (e.g., `"px_"`).
#' @param set_torque_identifier Character vector. Identifiers for torque data columns in the `.mot` file (e.g., `"tx_"`).
#' @param data_source_name Character. Name of the `.mot` file providing the external load data.
#' @param write_file Logical. If `TRUE`, writes the XML file to disk (default: `TRUE`).
#' @param return_object Logical. If `TRUE`, returns the `ExternalLoads` OpenSim object (default: `FALSE`).
#'
#' @details
#' Each external load set in OpenSim must specify:
#' * a **body** the load is applied to,
#' * **force/point/torque identifiers** (prefixes in the `.mot` file),
#' * and the **data source** (e.g., GRF `.mot` file).
#'
#' This function automates building such XML configurations, supporting multiple load sets (e.g., left and right foot).
#'
#' @return Invisibly returns the `opensim::ExternalLoads` object if `return_object = TRUE`.
#' @importFrom reticulate import
#' @export
setup_external_loads_osim <- function(setup_filename = "external_loads.xml",
                                      set_name = c("right","left"),
                                      set_applied_body_name = c("calcn_r", "calcn_l"),
                                      force_expressed_in_body_name = "ground",
                                      point_expressed_in_body_name = "ground",
                                      set_force_identifier = c("X1_force_", "X2_force_"),
                                      set_point_identifier = c("X1_point_", "X2_point_"),
                                      set_torque_identifier = c("X1_torque_", "X2_torque_"),
                                      data_source_name = "grf.mot",
                                      write_file = TRUE,
                                      return_object = FALSE){

  osim <- reticulate::import("opensim")

  #setup base objects
  extLoads = osim$ExternalLoads()
  exf = osim$ExternalForce()

  #update to loop correctly if needed
  for(i in 1:length(set_name)){

    #set name
    exf$setName(set_name[i])

    #set applies force
    exf$set_appliesForce(TRUE)

    #set applied body name
    exf$setAppliedToBodyName(set_applied_body_name[i])

    #set fore and point expression
    exf$setForceExpressedInBodyName(force_expressed_in_body_name)
    exf$setPointExpressedInBodyName(point_expressed_in_body_name)

    #set identifiers
    exf$setForceIdentifier(set_force_identifier[i])
    exf$setPointIdentifier(set_point_identifier[i])
    exf$setTorqueIdentifier(set_torque_identifier[i])

    #set data source
    exf$set_data_source_name(data_source_name)

    #add to ext loads
    extLoads$cloneAndAppend(exf)
  }

  #add data soruce to whole object
  extLoads$setDataFileName(data_source_name)

  #write to file
  if(write_file){
    extLoads$printToXML(setup_filename)
  }

  #return
  if(return_object){
    return(extLoads)
  }

}
