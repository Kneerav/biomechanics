#' Resample a numeric data frame (where the first column represents time) to the specified number of nodes
#'
#' This function resamples a numeric data frame to the specified number of nodes using interpolation.
#'
#' @param data A numeric data frame where each column is a time series to be resampled. The first column should be a time vector.
#' @param nodes Integer, the number of nodes to which the data should be resampled. Default is 101.
#' @param method Character, the interpolation method to use. Default is "cubic". Options include "linear", "nearest", "spline", "pchip", etc.
#' @return A data frame with the same number of columns as `data`, but with each column resampled to the specified number of nodes.
#' @importFrom pracma interp1
#' @export
#' @examples
#' # Create a sample data frame
#' df <- data.frame(time = 1:10, value = sin(1:10))
#' # Resample the data frame to 50 nodes
#' resampled_df <- time_normalise(df, nodes = 50)
time_normalise <- function(data, 
                           nodes = 101, 
                           method = "cubic") {
  
  # Function to resample the data
  Normal_cycle <- function(x, y) {
    n <- length(x)
    t0 <- y[, 1]
    t1 <- seq(t0[1], t0[n], length.out = nodes)
    y1 <- pracma::interp1(t0, x, t1, method = method)
    return(y1)
  }
  
  # Apply the resampling to each column in the data frame
  normalised_df <- as.data.frame(apply(data, 2, Normal_cycle, y = data))
  
  # Return the normalised data frame
  return(normalised_df)
}
