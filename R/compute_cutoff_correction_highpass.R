#' Compute Corrected Cutoff Frequency for Highpass Filter
#'
#' This function computes the corrected cutoff frequency for a highpass filter
#' using the correction factor by David A. Winter and Robertson's high-pass modification.
#'
#' @param filter_passes Integer. The number of passes of the filter. Default is 2.
#' @param cutoff_frequency Numeric. The cutoff frequency in Hz. Default is 20.
#' @param sampling_frequency Numeric. The sampling frequency in Hz. Default is 200.
#' @return Numeric. The corrected cutoff frequency in Hz.
#' @export
compute_cutoff_correction_highpass = function(filter_passes = 2,
                                              cutoff_frequency = 20,
                                              sampling_frequency = 200){

  # Robertson high-pass modification
  cutoff_frequency <- sampling_frequency/2 - cutoff_frequency

  # David A. Winter butterworth correction factor
  correction <- (((2^(1/filter_passes))-1)^(1/4))

  # angular cutoff frequency
  angular_cutoff <- 2*pi*cutoff_frequency

  # adjusted angular cutoff frequency
  adjusted_angular_cutoff <- tan(angular_cutoff/(2*sampling_frequency))

  # David A. Winter correction
  corrected_angular_cutoff <- adjusted_angular_cutoff * correction

  # corrected cutoff frequency
  corrected_frequency_cutoff <- atan(corrected_angular_cutoff)*sampling_frequency/pi

  # return object
  return(corrected_frequency_cutoff)
}
