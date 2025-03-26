#' Resample a numeric data frame (where the first column represents time) to the specified sample rate
#'
#' This function resamples a numeric data frame to the specified sample rate using interpolation.
#'
#' @param data A numeric data frame where each column is a time series to be resampled. The first column should be a time vector.
#' @param sample_rate_output Integer, the desired sample rate (Hz) of the output signal. Default is 100Hz.
#' @param method Character, the interpolation method to use. Default is "cubic". Options include "linear", "nearest", "spline", "pchip", etc.
#' @return A data frame with the same number of columns as `data`, but with each column resampled to the specified sample rate.
#' @importFrom pracma interp1
#' @export
resample <- function(data,
                     sample_rate_output = 100,
                     method = "cubic") {

  # Function to resample the data
  Normal_cycle <- function(x, y) {
    n <- length(x)
    t0 <- y[, 1]
    t1 <- seq(t0[1], t0[n], by = 1/sample_rate_output)
    y1 <- pracma::interp1(t0, x, t1, method = method)
    return(y1)
  }

  # Apply the resampling to each column in the data frame
  normalised_df <- as.data.frame(apply(data, 2, Normal_cycle, y = data))

  # Return the normalised data frame
  return(normalised_df)
}
