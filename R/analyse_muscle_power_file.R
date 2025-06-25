#' Analyse muscle power
#'
#' This function computes muscle power as the product of muscle force and muscle lengthening velocity.
#' It includes an option to reverse the velocity signal to treat shortening as positive (useful for power generation interpretation).
#'
#' @param muscle_force_file A string specifying the file path for the muscle force data (e.g., "muscle_forces.sto").
#' @param muscle_velocity_file A string specifying the file path for the muscle lengthening velocities (e.g., "muscle_velocities.sto").
#' @param file_output A string specifying the output file path for the muscle power results (e.g., "muscle_power.sto").
#' @param reverse_velocity A logical indicating whether to reverse the sign of velocity (default is FALSE).
#' @param write_file A logical indicating whether to write the results to a file (default is TRUE).
#' @param return_object A logical indicating whether to return the resulting data frame (default is FALSE).
#'
#' @return If return_object = TRUE, returns a data frame with the computed muscle power. If write_file = TRUE, writes the data to a file.
#' @export
#'
#' @import dplyr
#' @importFrom stringr str_replace_all

analyse_muscle_power_file <- function(muscle_force_file = "muscle_forces.sto",
                                 muscle_velocity_file = "muscle_velocities.sto",
                                 file_output = "muscle_power.sto",
                                 reverse_velocity = FALSE,
                                 write_file = TRUE,
                                 return_object = FALSE) {
  
  # Ensure required packages are loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  
  # Read data
  forces <- read_mot_sto(muscle_force_file)
  vels <- read_mot_sto(muscle_velocity_file)
  
  # Check dimensions match
  if (ncol(forces) != ncol(vels) || nrow(forces) != nrow(vels)) {
    stop("The number of rows or columns in the muscle force and velocity files do not match.")
  }
  
  # Optionally reverse velocity
  if (reverse_velocity) {
    vels[ , -1] <- -vels[ , -1]  # Assuming first column is time
  }
  
  # Compute power for each muscle
  power_list <- list()
  for (force_i in 2:ncol(forces)) {
    
    #get forces column
    force_col_i = forces[force_i]
    
    #get matching velocity column
    vel_col_i = vels[colnames(force_col_i)]
    
    #compute and store power
    power_list[[force_i - 1]] <- force_col_i * vel_col_i
  }
  
  # Combine to data frame
  power_df <- data.frame(time = forces[[1]], do.call(cbind, power_list))
  colnames(power_df) <- colnames(forces)
  
  # Write to file if needed
  if (write_file) {
    write_mot_sto(power_df, "Muscle power", "no", file_output)
  }
  
  # Return result if requested
  if (return_object) {
    return(power_df)
  }
}
