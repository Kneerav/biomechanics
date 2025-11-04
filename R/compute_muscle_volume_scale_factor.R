#' Calculate Strength Scale Factor Based on Height and Mass
#'
#' This function computes a muscle volume scale factor based on an individual's height
#' and mass relative to reference values from Rajagopal et al. (2016) using regression equations
#' from Handsfield et al.
#'
#' @param height A numeric value representing the height of the individual in meters. If NULL, only mass will be used.
#' @param mass A numeric value representing the mass of the individual in kilograms.
#' @param height_ref A numeric value for the reference height in meters (default is 1.68).
#' @param mass_ref A numeric value for the reference mass in kilograms (default is 75.337).
#'
#' @references
#' Handsfield GG, Meyer CH, Hart JM, Abel MF, Blemker SS. Relationships of 35 lower limb muscles to height and body mass quantified using MRI. J Biomech. 2014 Feb 7;47(3):631-8. doi: 10.1016/j.jbiomech.2013.12.002.
#'
#' @return A numeric value representing the calculated strength scale factor.
#' @export
compute_muscle_volume_scale_factor <- function(height = NULL,
                                mass,
                                height_ref = 1.68, # (m) Rajagopal et al. (2016)
                                mass_ref = 75.337){ # (kg) Rajagopal et al. (2016)

  # Equation based on Handsfield ratio to standard model
  if(is.null(height)){
    volumeScale <- (91 * mass + 588) / (91 * mass_ref + 588)
  } else{
    volumeScale <- (47.0 * mass * height + 1285.0) / (47.0 * mass_ref * height_ref + 1285.0)
  }

  # Get scale factor
  return(volumeScale)

}
