#' Analyse Angular Momentum for a Model
#'
#' This function computes the segmental angular momentum about the centre of mass of a body model based on position
#' and velocity kinematics over time, returning the results or saving them to a file.
#'
#' @param model_file A string specifying the model file (in .osim format).
#' @param body_pos_file A string specifying the body position file (in .sto format).
#' @param body_vel_file A string specifying the body velocity file (in .sto format).
#' @param file_output A string specifying the output file for the angular momentum results.
#' @param in_degrees Logical; if TRUE, input angles and velocities are assumed to be in degrees. Defaults to TRUE.
#' @param write_file Logical; if TRUE, the function writes results to a file. Defaults to TRUE.
#' @param return_object Logical; if TRUE, the function returns the computed angular momentum as a data frame.
#'
#' @return If return_object = TRUE, a data frame containing the angular momentum values for each body at each time step.
#'
#' @importFrom reticulate import
#' @importFrom dplyr select
#' @importFrom pracma Diag inv cross deg2rad
#'
#' @export
analyse_angular_momentum_com = function(model_file = "Model_SCALED.osim",
                                        body_pos_file = "Model_scaled_BodyKinematics_pos_global.sto",
                                        body_vel_file = "Model_scaled_BodyKinematics_vel_global.sto",
                                        file_output = "angular_momentum.sto",
                                        in_degrees = TRUE,
                                        write_file = TRUE,
                                        return_object = FALSE){

  #read in r packages
  require(reticulate)
  require(dplyr)
  require(pracma)

  # Check if 'opensim' and 'scipy' modules are available in the Python environment
  if (!reticulate::py_module_available("opensim")) {
    stop("Error: The 'opensim' Python package is not installed. Please install it and try again.")
  }

  # if (!reticulate::py_module_available("scipy")) {
  #   stop("Error: The 'scipy' Python package is not installed. Please install it and try again.")
  # }

  #read in python packages
  osim = import("opensim")
  #scipy = import("scipy") #no longer needed

  #get model and bodies
  model = osim$Model(model_file)
  bodyset = model$getBodySet()
  nbods = bodyset$getSize()

  #get body kinematics
  pos = read_mot_sto(body_pos_file)
  vel = read_mot_sto(body_vel_file)

  #get centre of mass
  pos_com = pos %>% select(center_of_mass_X, center_of_mass_Y, center_of_mass_Z)
  vel_com = vel %>% select(center_of_mass_X, center_of_mass_Y, center_of_mass_Z)

  #setup storage
  full=list()

  #start loop
  for(i in 1L:nbods){

    #get body inertial info
    body_i = bodyset$get(i-1L)
    body_i_name = body_i$getName()
    mass_i = body_i$get_mass()
    inertia_i = c(body_i$getInertia()$getMoments()$get(0L),
                  body_i$getInertia()$getMoments()$get(1L),
                  body_i$getInertia()$getMoments()$get(2L))

    #get body orientation and angular velocities
    o_i = pos %>% select(contains(body_i_name) & contains("_O"))
    w_i = vel %>% select(contains(body_i_name) & contains("_O"))

    #get positions and velocities
    q_i = pos %>% select(contains(body_i_name) & !(contains("_O")))
    v_i = vel %>% select(contains(body_i_name) & !(contains("_O")))

    #get relative positions and velocities
    r_i = q_i - pos_com
    vr_i = v_i - vel_com

    #get inertia matrix
    I_i = pracma::Diag(inertia_i)

    #setup storage
    body_temp = matrix(ncol=3, nrow=nrow(pos))

    #loop through samples
    for(nsamp in 1:nrow(pos)){

      #get kinematics at current sample, converting to radians where necessary
      if(in_degrees){
        theta3 = apply(o_i %>% slice(nsamp), 2, pracma::deg2rad)
        omega3 = apply(w_i %>% slice(nsamp), 2, pracma::deg2rad)
      } else {
        theta3 = apply(o_i %>%  slice(nsamp), 2, max)
        omega3 = apply(w_i %>%  slice(nsamp), 2, max)
      }

      v_3 = apply(v_i %>% slice(nsamp), 2, max)
      r_3 = apply(r_i %>% slice(nsamp), 2, max)

      #get rotation matrix. Replaced scipy dependency
      #rot_i = scipy$spatial$transform$Rotation$from_rotvec(theta3, degrees=FALSE)
      rot_i = create_rotation_matrix_from_vector(theta3, in_degrees=FALSE)
      rotmat = rot_i$as_matrix()

      # transform I_i into inertial coords via R * I_i * R_inv
      I_inertial = rotmat %*% (I_i %*% pracma::inv(rotmat))

      # Instantaneous segmental angular momentum relative to centre-of mass:
      #   L_seg_t = r x mv + Iw
      L_seg_t = pracma::cross(r_3, mass_i * v_3) + t(I_inertial %*% omega3)

      # ensure accurate naming
      colnames(body_temp) <- c(paste0(body_i_name, "_x"),paste0(body_i_name, "_y"),paste0(body_i_name, "_z"))

      # store
      body_temp[nsamp,] = L_seg_t
    }

    #add to list
    full[[i]] = body_temp

  }

  #create data frame
  full_df = do.call("cbind.data.frame", full)

  #write to file
  if(write_file == TRUE){
    write_mot_sto(full_df, "Angular momentum", "no", file_output)
  }

  #return object
  if(return_object == TRUE){
    return(full_df)
  }

}
