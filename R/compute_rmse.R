#' Compute Root Mean Square Error (RMSE) with Normalization Options (untested)
#'
#' This function calculates the Root Mean Square Error (RMSE) between two numeric vectors (signals) and offers options for normalizing the RMSE.
#'
#' @param observed A numeric vector of observed values (true values).
#' @param predicted A numeric vector of predicted values (model values).
#' @param normalise A character string specifying the normalization method. Options are "none", "max", "min", "range", "median", "mean" or "sd". Default is "none".
#' @return A numeric value representing the normalised RMSE between the observed and predicted values.
#' @importFrom stats median sd
#' @export
#' @examples
#' # Example observed and predicted signals
#' observed <- c(1.0, 2.0, 3.0, 4.0, 5.0)
#' predicted <- c(1.1, 2.1, 2.9, 4.2, 5.0)
#' # Compute RMSE without normalization
#' rmse_no_norm <- compute_rmse(observed, predicted, normalise = "none")
#' print(rmse_no_norm)
#' # Compute RMSE normalised by the max value of the observed signal
#' rmse_max <- compute_rmse(observed, predicted, normalise = "max")
#' print(rmse_max)
compute_rmse <- function(observed, predicted, normalise = "none") {

  # Check that input vectors have the same length
  if (length(observed) != length(predicted)) {
    stop("Observed and predicted vectors must have the same length.")
  }

  # Compute the RMSE
  mse <- mean((observed - predicted)^2)
  rmse <- sqrt(mse)

  # Normalization
  if (normalise == "none") {
    return(rmse)
  }

  # Check if the observed vector is not empty
  if (length(observed) == 0) {
    stop("Observed vector is empty.")
  }

  # Calculate normalization values based on the chosen method
  obs_min <- min(observed)
  obs_max <- max(observed)
  obs_range <- obs_max - obs_min
  obs_median <- median(observed)
  obs_mean <- mean(observed)
  obs_sd <- sd(observed)

  # Apply normalization
  normalised_rmse <- switch(normalise,
                            "none" = rmse,
                            "max" = rmse / obs_max,
                            "min" = rmse / obs_min,
                            "range" = rmse / obs_range,
                            "median" = rmse / obs_median,
                            "mean" = rmse / obs_mean,
                            "sd" =rmse / obs_sd,
                            stop("Invalid normalization method. Choose from 'none', 'max', 'min', 'range', 'median', or 'mean'.")
  )

  return(normalised_rmse)
}
