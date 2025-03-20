#' Scale Muscle Strength in OpenSim Model
#'
#' This function scales the muscle strength of an OpenSim model by a specified factor. It can 
#' save the modified model to a file or return the updated model object.
#'
#' @param model_input_file A string specifying the path to the input OpenSim model file (default is "Baseline_model.osim").
#' @param model_output_file A string specifying the path for the output updated OpenSim model file (default is "Baseline_model_stronger.osim").
#' @param write_file A logical value indicating whether to write the updated model to a file (default is TRUE).
#' @param return_object A logical value indicating whether to return the updated model object (default is FALSE).
#' @param scaleFactor A numeric value indicating the scaling factor for muscle strength (default is 5).
#' 
#' @return If `return_object` is TRUE, returns the modified OpenSim model object; otherwise, NULL.
#' @importFrom reticulate import
#' @export
strength_scale_osim = function(model_input_file = "Baseline_model.osim",
                               model_output_file = "Baseline_model_stronger.osim",
                               write_file = TRUE,
                               return_object = FALSE,
                               scaleFactor = 5){
  
  # Check if reticulate is installed
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is not installed. Please install it to use this function.")
  }
  
  # Load OpenSim via reticulate
  osim <- NULL
  tryCatch({
    osim <- reticulate::import("opensim")
  }, error = function(e) {
    stop("Failed to import 'opensim' module. Please ensure that the OpenSim Python package is installed and accessible.")
  })
  
  # Proceed with the model manipulation
  
  # Load in default model
  Model1 <- osim$Model(model_input_file)
  Model1$initSystem()
  
  # Create modified model
  Model2 <- osim$Model(Model1)
  Model2$initSystem()
  Model2$setName('modelModified')
  
  # get muscles and number of muscle for each model
  Muscles1 <- Model1$getMuscles() 
  nMuscles <- Muscles1$getSize()
  Muscles2 <- Model2$getMuscles()
  nMuscles2 <- Muscles2$getSize()
  
  # update the strength in the modified model by the scale factor
  for(i in 1L:nMuscles){
    currentMuscle <- Muscles1$get(i - 1L)
    newMuscle <- Muscles2$get(i - 1L)
    newMuscle$setMaxIsometricForce(currentMuscle$getMaxIsometricForce() * scaleFactor)
  }
  
  # write to file
  if(write_file == TRUE){
    Model2$printToXML(model_output_file)  
  }
  
  # return object
  if(return_object == TRUE){
    return(Model2)
  }
  
  # Return NULL invisibly if neither write nor return is requested
  invisible(NULL)  
}
