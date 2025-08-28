#' Adjust Reserve Actuators to Align with Pelvis Center of Mass
#'
#' This function modifies the point location of the first three reserve actuators in an
#' OpenSim force set file so they act through the center of mass of the pelvis body.
#'
#' @param model_file Character. Path to the OpenSim model file (`.osim`). Defaults to `"Model_Scaled.osim"`.
#' @param actuators_file Character. Path to the actuators XML file. Defaults to `"reserve_actuators.xml"`.
#' @param write_file Logical: If TRUE, writes the moment arm values to a file. Default is FALSE.
#' @param return_object Logical: If TRUE, returns the moment arm data as a data frame. Default is TRUE.
#'
#' @return If `return_object` is TRUE, a data frame containing the computed moment arm values. Otherwise, nothing is returned.
#'
#' @import reticulate
#' @export
adjust_reserve_actuators_osim = function(model_file = "Model_Scaled.osim",
                           actuators_file = "reserve_actuators.xml",
                           write_file = FALSE,
                           return_object = TRUE){


  # Check for reticulate
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required but not installed. Please install it using install.packages('reticulate').")
  }

  # Try to import OpenSim
  osim <- tryCatch(
    reticulate::import("opensim"),
    error = function(e) stop("Failed to import the 'opensim' Python module. Ensure it's installed and accessible.")
  )

  osimModel = osim$Model(model_file)
  pelvis = osimModel$getBodySet()$get("pelvis")
  massCenter = pelvis$getMassCenter();
  forceset = osim$ForceSet(actuators_file);

  for(actuator_i in 1L:3L){

    actuator_iL = actuator_i - 1L

    pointActuator = osim$PointActuator$safeDownCast(forceset$get(actuator_iL))
    pointActuator$set_point(massCenter)
  }

  # Write to file
  if(write_file == TRUE){
    forceset$printToXML(actuators_file)
  }

  # Return object
  if(return_object == TRUE){
    return(forceset)
  }

}
