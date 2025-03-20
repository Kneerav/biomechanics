#' Calculate Strength Scale Factor Based on Height and Mass
#'
#' This function computes a muscle volume scale factor based on an individual's height 
#' and mass relative to reference values from Rajagopal et al. (2016) using regression equations
#' from Handsfield et al.
#'
#' @param height A numeric value representing the height of the individual in meters.
#' @param mass A numeric value representing the mass of the individual in kilograms.
#' @param height_ref A numeric value for the reference height in meters (default is 1.68).
#' @param mass_ref A numeric value for the reference mass in kilograms (default is 75.337).
#' 
#' @return A numeric value representing the calculated strength scale factor.
#' @export
compute_muscle_volume_scale_factor <- function(height, 
                                mass, 
                                height_ref = 1.68, # (m) Rajagopal et al. (2016)
                                mass_ref = 75.337){ # (kg) Rajagopal et al. (2016)
  
  # Equation based on Handsfield ratio to standard model
  volumeScale <- (47.0 * mass * height + 1285.0) / (47.0 * mass_ref * height_ref + 1285.0)
  
  # Get scale factor
  return(volumeScale)
  
}
