#' Analyse joint power
#'
#' This function computes joint power using inverse dynamics and joint velocities from OpenSim. 
#' This function has the option to sum components for a given joint and limb (e.g., hip, knee) will create a new 
#' column for each joint (e.g., hip_r, hip_l).. By default, this summing excludes any columns that contain the 
#' word "beta" (i.e., from the couple patellofemoral joint).
#' 
#' @param inverse_dynamics_file A string specifying the file path for the inverse dynamics data (e.g., "inverse_dynamics.sto").
#' @param joint_vel_file A string specifying the file path for the joint velocities (e.g., "Model_scaled_Kinematics_u.sto").
#' @param file_out A string specifying the output file path for the joint power results (e.g., "joint_power.sto").
#' @param in_degrees A logical indicating whether the input joint velocities are in degrees (default is TRUE). If TRUE, velocities are converted to radians.
#' @param sum_joints A character vector of joints to sum (e.g., c("hip", "arm")). Default is c("hip", "arm").
#' @param sum_suffix A character vector of suffixes for left and right sides (e.g., c("_l", "_r")). Default is c("_l", "_r").
#' @param write_file A logical indicating whether to write the results to a file (default is TRUE).
#' @param return_object A logical indicating whether to return the resulting data frame (default is FALSE).
#'
#' @return If return_object = TRUE, returns a data frame with the computed joint power. If write_file = TRUE, writes the data to a file.
#' @export
#'
#' @import dplyr
#' @import pracma
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
analyse_joint_power = function(inverse_dynamics_file = "inverse_dynamics.sto",
                               joint_vel_file = "Model_scaled_Kinematics_u.sto",
                               file_out = "joint_power.sto",
                               in_degrees = TRUE,
                               sum_joints = c("hip", "arm"),
                               sum_suffix = c("_l", "_r"),
                               write_file = TRUE,
                               return_object = FALSE){
  
  
  # Ensure required packages are loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop("Package 'pracma' is required but not installed.")
  }
  
  #read in r packages
  require(dplyr)
  require(pracma)
  
  #get data
  moments = read_mot_sto(inverse_dynamics_file)
  vel = read_mot_sto(joint_vel_file)
  
  #change column names for moment
  colnames(moments) = stringr::str_replace_all(colnames(moments), "_moment", "")
  colnames(moments) = stringr::str_replace_all(colnames(moments), "_force", "")
  
  #test
  if (ncol(moments) != ncol(vel) || nrow(moments) != nrow(vel)) {
    stop("The number of rows or columns in the moments and velocities files do not match.")
  }
  
  #multiply
  power_i = list()
  
  for(joint_i in 2:ncol(moments)){
    
    #get moment column
    moment_col_i = moments[joint_i]
    
    #get matching column from vel
    vel_col_i = vel[colnames(moment_col_i)]
    
    #convert if needed
    if(in_degrees){
      vel_col_i = apply(vel_col_i, 2, pracma::deg2rad)
    }
    
    #power
    power_i[[joint_i-1]] = moment_col_i * vel_col_i
    
  }
  
  #bind to data frame
  power_raw = do.call("cbind", power_i)
  
  #combine joints if needed
  for (joint in sum_joints) {
    
    # Loop through each suffix for the side (left, right)
    for (suffix in sum_suffix) {
      
      # Create a pattern to identify columns (e.g., hip_l or hip_r)
      pattern <- paste0("^", joint, "_.*", suffix, "$")
      
      # Identify columns that match the pattern
      joint_columns <- grep(pattern, colnames(power_raw), value = TRUE)
      
      # Exclude any columns that contain the word "beta"
      joint_columns <- joint_columns[!grepl("beta", joint_columns)]
      
      # If any matching columns are found, sum them and create a new column
      if (length(joint_columns) > 0) {
        
        # Create a new column name (e.g., hip_r, hip_l)
        new_col_name <- paste0(joint, suffix)
        
        # Sum the selected columns and add them as a new column
        power_raw[[new_col_name]] <- rowSums(power_raw[, joint_columns, drop = FALSE], na.rm = TRUE)
      }
    }
  }
  
  #create data frame
  full_df = as.data.frame(power_raw)
  
  #write to file
  if(write_file == TRUE){
    write_mot_sto(full_df, "Joint power", "no", file_out)
  }
  
  #return object
  if(return_object == TRUE){
    return(full_df)
  }
  
}