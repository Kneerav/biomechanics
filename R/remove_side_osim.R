#' Remove Side from OpenSim Model
#'
#' This function removes actuators for a specified side (left or right) in an OpenSim model.
#'
#' @param model_input_file A string representing the path to the input OpenSim model file. Default is "Baseline_markers.osim".
#' @param model_output_file A string representing the path to the output OpenSim model file. Default is "Baseline_markers_removed.osim".
#' @param side A string indicating which side (left or right) of the body to remove the actuators for. Default is "l" (left).
#'
#' @return The function returns nothing, but writes the modified OpenSim model to `model_output_file`.
#'
#' @export
remove_side_osim = function(model_input_file = "Baseline_markers.osim",
                            model_output_file = "Baseline_markers_removed.osim",
                            side = "l"){

  #Check if reticulate is installed
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is not installed. Please install it to use this function.")
  }

  #Load OpenSim via reticulate
  osim <- NULL
  tryCatch({
    osim <- reticulate::import("opensim")
  }, error = function(e) {
    stop("Failed to import 'opensim' module. Please ensure that the OpenSim Python package is installed and accessible.")
  })

  #import model
  model = osim$Model(model_input_file)
  model$initSystem()

  #get body set
  forceset = model$updForceSet()

  #get number
  n_actuators = forceset$getSize()

  #create list
  muscle_list = list()

  #remove actuator
  for(i in 1L:n_actuators){

    #adjust iterator
    i_L = i - 1L

    #get current muscle name
    act_name_i = forceset$get(i_L)$getName()

    muscle_list[[i]] = act_name_i

  }

  #create removal list
  muscle_vec = unlist(muscle_list)
  muscle_vec_side = muscle_vec[stringr::str_detect(muscle_vec, paste0("_", side, "$"), negate = F)]

  #remove actuator
  for(i in 1L:length(muscle_vec_side)){

    #get current muscle name
    forceset$remove(forceset$getIndex(muscle_vec_side[i]))


  }

  #finalise connections
  model$finalizeConnections()

  #write
  model$printToXML(model_output_file)

}
