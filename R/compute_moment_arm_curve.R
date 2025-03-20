#' Compute Moment Arm Curve for a Muscle-Coordinate Pair in an OpenSim Model
#'
#' This function computes the moment arm curve for a specified muscle and coordinate in an OpenSim model, iterating over the coordinate's range.
#' The moment arm is calculated at increments of a specified angle and can be written to a file or returned as a data frame.
#'
#' @param model_input_file A string representing the path to the OpenSim model file. Default is "../TestData/Model_SCALED.osim".
#' @param coordinate A string indicating the name of the coordinate of interest. Default is "knee_angle_r".
#' @param muscle A string indicating the name of the muscle of interest. Default is "vasmed_r".
#' @param file_output A string representing the file path to write the moment arm data. Default is "vasmed_r_knee_angle_r_moment_arm.sto".
#' @param deg_increment The angular increment (in degrees) for which the moment arm will be computed. Default is 10.
#' @param write_file Logical: If TRUE, writes the moment arm values to a file. Default is FALSE.
#' @param return_object Logical: If TRUE, returns the moment arm data as a data frame. Default is TRUE.
#'
#' @return If `return_object` is TRUE, a data frame containing the computed moment arm values. Otherwise, nothing is returned.
#'
#' @import reticulate
#'
#' @export
compute_moment_arm_curve = function(model_input_file = "../TestData/Model_SCALED.osim",
                                coordinate = "knee_angle_r",
                                muscle = "vasmed_r",
                                file_output = "vasmed_r_knee_angle_r_moment_arm.sto",
                                deg_increment = 10,
                                write_file = FALSE,
                                return_object = TRUE){

  require(dplyr)

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

  #import model
  model = osim$Model(model_input_file)
  model$initSystem()

  #get coords
  coord_set = model$getCoordinateSet()
  muscle_set = model$getMuscles()

  #sizes
  numCoords = coord_set$getSize()
  numMuscles = muscle_set$getSize()

  #storage prep
  coord_name_list = list()
  coord_traj_list = list()

  #re-initialise state
  state = model$initSystem()

  #get coord and muscle of interest
  coord_i = coord_set$get(coordinate)
  muscle_i = muscle_set$get(muscle)

  #get range
  coord_i_min = coord_i$get_range(0L) %>% pracma::rad2deg() %>% round(., 0)
  coord_i_max = coord_i$get_range(1L) %>% pracma::rad2deg() %>% round(., 0)

  #create coord trajectory
  coord_traj = seq(from = coord_i_min, to = coord_i_max, by = deg_increment)

  moment_arm_values = matrix(nrow = length(coord_traj), ncol = 2)
  for(coord_increment in 1:length(coord_traj)){

    #set position
    coord_i$setValue(state, pracma::deg2rad(coord_traj[coord_increment]), TRUE)

    #compute moment arm and store
    moment_arm_values[coord_increment,1] = coord_traj[coord_increment]
    moment_arm_values[coord_increment,2] = muscle_i$computeMomentArm(state, coord_i)

  }

  #create data.frame
  moment_arm_df = moment_arm_values %>%
    as.data.frame() %>%
    `colnames<-`(c(coord_i$getName(), muscle_i$getName()))

  # Write to file
  if(write_file == TRUE){
    write_mot_sto(moment_arm_df,
                                name = "Kinematics",
                                inDegrees = "yes",
                                file_output)
  }

  # Return object
  if(return_object == TRUE){
    return(moment_arm_df)
  }


}

