#' Filter Markers with Padding to Avoid Artifacts
#'
#' This function filters the signal using a low-pass Butterworth filter with padding at the start and end of the data to avoid artifacts.
#'
#' @param signal A numeric vector containing signal to be filtered.
#' @param cut_off Numeric. The cut-off frequency for the low-pass filter in Hz.
#' @param sample_rate Numeric. The sample rate of the signal in Hz.
#' @param order Numeric. The order of the Butterworth filter.
#' @param pad_size Numeric. The number of data points to pad at the start and end of the signal.
#' @return A numeric vector with the filtered signal, with padding removed.
#' @importFrom signal butter filtfilt
#' @export
zerolag_lowpass_filter <- function(signal,
                               cut_off = 6,
                               sample_rate = 120,
                               order = 2,
                               pad_size = 500) {
  
  # Check if the signal has enough elements to pad
  if (length(signal) <= pad_size) {
    stop("signal must have more elements than the padding size.")
  }
  
  # Replicate the first and last values for padding
  pad_start <- rep(signal[1], pad_size)
  pad_end <- rep(signal[length(signal)], pad_size)
  
  # Pad the signal
  padded_signal <- c(
    pad_start,
    signal,
    pad_end
  )
  
  # Filter the signal
  bw <- signal::butter(n = order, W = cut_off / (sample_rate / 2), plane = "z", type = "low")
  filtered_padded <- signal::filtfilt(bw, x = padded_signal)
  
  # Remove padding
  filtered_signal <- filtered_padded[(pad_size + 1):(length(filtered_padded) - pad_size)]
  
  return(filtered_signal)
}
