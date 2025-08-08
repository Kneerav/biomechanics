#' Replace Side Torque in OpenSim Model
#'
#' This function removes actuators for a specified side (left or right) in an OpenSim model and
#' adds new coordinate actuators (torques) to the corresponding side for certain body parts (e.g., hip, knee, and ankle).
#' Locked joints are automatically skipped.
#'
#' @param model_input_file A string representing the path to the input OpenSim model file. Default is "Baseline_markers.osim".
#' @param model_output_file A string representing the path to the output OpenSim model file. Default is "Baseline_markers_removed.osim".
#' @param side A string indicating which side (left or right) of the body to remove the actuators for. Default is "l" (left).
#'
#' @return The function returns nothing, but writes the modified OpenSim model to `model_output_file`.
#'
#' @export
replace_side_torque_osim = function(model_input_file = "Baseline_markers.osim",
                            model_output_file = "Baseline_markers_removed.osim",
                            side = "l",
							optimal_force = 10){

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

  #get force set
  forceset = model$updForceSet()

  #get number
  n_actuators = forceset$getSize()

  #create list
  muscle_list = list()

  #remove actuator
  for(i in 1L:n_actuators){

    #adjust iterator
    i_L = i - 1L

    #get actuator
	act_i = forceset$get(i_L)
	
	#get current muscle name
	act_name_i = act_i$getName()
	
	#num properties (to detect torque or force actuators, not the most robust)
	num_properties = act_i$getNumProperties()
	
	if(num_properties < 20){
	next
	}

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

  #get body set
  coordset = model$getCoordinateSet()

  #get number
  n_coords = coordset$getSize()

  #create list
  coord_list = list()

  #remove actuator
  for(i in 1L:n_coords){

    #adjust iterator
    i_L = i - 1L

    #skip if coord is locked
    if(coordset$get(i_L)$get_locked()){
      next
    }

    #get current muscle name
    coord_name_i = coordset$get(i_L)$getName()

    #append to list
    coord_list[[i]] = coord_name_i

  }

  #create removal list
  coord_vec = unlist(coord_list)
  coord_vec_side = coord_vec[stringr::str_detect(coord_vec, paste0("_", side, "$"), negate = F)]

  #create coordinate actuators for hip, knee and ankle
  for(i in 1L:length(coord_vec_side)){

    coordActuator = osim$CoordinateActuator()
    coordActuator$setName(paste0(coord_vec_side[i], "_torque"))
	coordActuator$set_max_control(Inf)
	coordActuator$set_min_control(-Inf)
    coordActuator$setOptimalForce(optimal_force)

    coord_i = coordset$get(coord_vec_side[i])
    coordActuator$setCoordinate(coord_i)

    #add actuator
    forceset$append(coordActuator)

  }

  #finalise connections
  model$finalizeConnections()

  #write
  model$printToXML(model_output_file)

}
