#' Update Maximum Contraction Velocity in an OpenSim Model
#'
#' This function loads an OpenSim model, modifies the maximum contraction velocity
#' of all muscles, and optionally writes the modified model to a file or returns it.
#'
#' @param model_input_file Path to the input `.osim` model file. Defaults to `"Baseline_model.osim"`.
#' @param model_output_file Path to the output `.osim` file. Defaults to `"Baseline_model_stronger.osim"`.
#' @param new_max Numeric value specifying the new maximum contraction velocity for all muscles. Defaults to `20`.
#' @param write_file Logical; whether to write the modified model to a file. Defaults to `TRUE`.
#' @param return_object Logical; whether to return the modified model object. Defaults to `FALSE`.
#'
#' @return If `return_object = TRUE`, returns the modified OpenSim model object. Otherwise, returns `NULL` invisibly.
#'
#' @import reticulate
#'
#' @export
update_max_contraction_velocity_osim = function(model_input_file = "Baseline_model.osim",
                                                model_output_file = "Baseline_model_stronger.osim",
                                                new_max = 20,
                                                write_file = TRUE,
                                                return_object = FALSE){

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
    newMuscle$setMaxContractionVelocity(new_max)
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
