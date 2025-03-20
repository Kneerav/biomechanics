#' Compute Corrected Cutoff Frequency for Filter
#'
#' This function computes the corrected cutoff frequency for a lowpass or highpass filter
#' using the correction factor by David A. Winter. For highpass filters, Robertson's
#' high-pass modification is applied.
#'
#' @param filter_passes Integer. The number of passes of the filter. Default is 2.
#' @param cutoff_frequency Numeric. The cutoff frequency in Hz. Default is 6 for lowpass.
#' @param sampling_frequency Numeric. The sampling frequency in Hz. Default is 200.
#' @param type Character. The type of filter. Must be either "lowpass" or "highpass". Default is "lowpass".
#' @return Numeric. The corrected cutoff frequency in Hz.
#' @export
compute_cutoff_correction <- function(filter_passes = 2,
                                      cutoff_frequency = 20,
                                      sampling_frequency = 200,
                                      type = "lowpass") {

  # Validate type argument
  if (!type %in% c("lowpass", "highpass")) {
    stop("Invalid value for 'type'. Must be either 'lowpass' or 'highpass'.")
  }

  # Apply Robertson high-pass modification if type is "highpass"
  if (type == "highpass") {
    cutoff_frequency <- sampling_frequency/2 - cutoff_frequency
  }

  # David A. Winter butterworth correction factor
  correction <- (((2^(1/filter_passes))-1)^(1/4))

  # Angular cutoff frequency
  angular_cutoff <- 2*pi*cutoff_frequency

  # Adjusted angular cutoff frequency
  adjusted_angular_cutoff <- tan(angular_cutoff/(2*sampling_frequency))

  # Apply correction factor
  if (type == "lowpass") {
    corrected_angular_cutoff <- adjusted_angular_cutoff / correction
  } else { # highpass
    corrected_angular_cutoff <- adjusted_angular_cutoff * correction
  }

  # Corrected cutoff frequency
  corrected_frequency_cutoff <- atan(corrected_angular_cutoff)*sampling_frequency/pi

  # Return object
  return(corrected_frequency_cutoff)
}
