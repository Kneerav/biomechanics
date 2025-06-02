#' Compute Area Under the Curve (AUC) Using Trapezoidal Integration
#'
#' This function calculates the area under the curve (AUC) using the trapezoidal rule from the `pracma` package.
#' It also supports computing the positive AUC (sum of areas above the x-axis) and negative AUC (sum of areas below the x-axis).
#'
#' @param time A numeric vector representing the x-coordinates of the data points, typically time in seconds.
#' @param variable A numeric vector representing the y-coordinates of the data points, typically a kinetic quantity like force or moment.
#' @param type A character string specifying the type of AUC to compute. Options are "total", "positive", or "negative". Default is "total".
#' @return A numeric value representing the computed AUC based on the specified type.
#' @importFrom pracma trapz
#' @export
#' @examples
#' # Example time and variable data
#' time <- c(1, 2, 3, 4, 5)
#' variable <- c(-2, 3, 5, -7, 6)
#' # Compute total AUC
#' auc_total <- compute_impulse(time, variable, type = "total")
#' print(auc_total)
#' # Compute positive AUC
#' auc_positive <- compute_impulse(time, variable, type = "positive")
#' print(auc_positive)
#' # Compute negative AUC
#' auc_negative <- compute_impulse(time, variable, type = "negative")
#' print(auc_negative)
compute_impulse <- function(time, variable, type = "total") {

  # Check that input vectors have the same length
  if (length(time) != length(variable)) {
    stop("time and variable vectors must have the same length.")
  }

  # Ensure the pracma package is available
  if (!requireNamespace("pracma", quietly = TRUE)) {
    stop("The 'pracma' package is required but not installed. Please install it using install.packages('pracma').")
  }

  # Initialize auc variable
  auc <- 0

  # Compute the area based on the type
  if (type == "total") {
    auc <- pracma::trapz(time, variable)
  } else if (type == "positive") {
    # Compute positive AUC
    positive_y <- ifelse(variable > 0, variable, 0)
    auc <- pracma::trapz(time, positive_y)
  } else if (type == "negative") {
    # Compute negative AUC
    negative_y <- ifelse(variable < 0, variable, 0)
    auc <- pracma::trapz(time, negative_y)
  } else {
    stop("Invalid type specified. Choose from 'total', 'positive', or 'negative'.")
  }

  return(auc)
}
