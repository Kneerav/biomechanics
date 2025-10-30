#' Determine Optimal Filter Cutoff from Residual Analysis
#'
#' This function estimates the optimal low-pass filter cutoff frequency
#' from a residual analysis curve, based on the linear region of the RMSE–cutoff plot.
#'
#' @param rmses Numeric vector. RMSE values computed from `residual_analysis_curve()`.
#' @param cutoff_vec Numeric vector. Cutoff frequencies corresponding to each RMSE value.
#' @param linear_cutoff_region Numeric vector of length 2. The lower and upper cutoff limits (Hz)
#' used for fitting the linear tail of the RMSE curve. Default is `c(15, 20)`.
#'
#' @details
#' A linear regression is fitted to the high-frequency “tail” of the RMSE curve.
#' The y-intercept of this regression line defines the residual threshold.
#' The optimal cutoff frequency is then the last frequency before RMSE exceeds this threshold.
#'
#' @references
#' Winter, David A. Biomechanics and motor control of human movement. John Wiley & Sons, 2009.
#'
#' @return Numeric value representing the estimated optimal cutoff frequency (Hz).
#'
#' @importFrom ggplot2 ggplot geom_point aes geom_abline theme_classic xlab ylab
#' @export
residual_analysis_opt_cutoff <- function(rmses, cutoff_vec, linear_cutoff_region = c(15,20)) {

  #check inputs
  if (length(rmses) != length(cutoff_vec)) {
    stop("`rmses` and `cutoff_vec` must be the same length.", call. = FALSE)
  }

  # Ensure that the linear_cutoff_region is valid
  if (length(linear_cutoff_region) != 2 || linear_cutoff_region[1] >= linear_cutoff_region[2]) {
    stop("linear_cutoff_region must be a vector of length 2 with the first value smaller than the second.")
  }

  # Find the indices that correspond to the linear_cutoff_region
  lower_idx <- which(cutoff_vec >= linear_cutoff_region[1])[1]  # The first index where cutoff_vec >= lower limit
  upper_idx <- which(cutoff_vec <= linear_cutoff_region[2])[length(which(cutoff_vec <= linear_cutoff_region[2]))]  # The last index where cutoff_vec <= upper limit

  if (is.na(lower_idx) || is.na(upper_idx) || lower_idx >= upper_idx) {
    stop("Invalid `linear_cutoff_region`: no matching data points found.", call. = FALSE)
  }

  # Extract tail data from the specified range
  tail_cutoff_vec <- cutoff_vec[lower_idx:upper_idx]
  tail_rmses <- rmses[lower_idx:upper_idx]

  # Perform linear regression on the tail end of the RMSE plot
  lm_fit <- lm(tail_rmses ~ tail_cutoff_vec)

  # Threshold line (y-intercept of the regression line)
  threshold <- coef(lm_fit)[1]
  slope <- coef(lm_fit)[2]

  g <- ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(cutoff_vec, rmses), pch=21, fill="#024950")+
    ggplot2::theme_classic()+
    ggplot2::ylab("RMSE")+
    ggplot2::xlab("Cutoff frequency (Hz)")+
    ggplot2::geom_abline(intercept = threshold, slope = slope, col="grey", lty="51")+
    ggplot2::geom_abline(intercept = threshold, col="grey", lty="51")

  plot(g)

  # Find the first cutoff frequency where RMSE exceeds the threshold
  optimal_cutoff_idx <- max(which(rmses > threshold))
  optimal_cutoff <- cutoff_vec[optimal_cutoff_idx]

  # Return the optimal cutoff frequency
  message("Estimated optimal cutoff frequency: ", round(optimal_cutoff, 2), " Hz")

  invisible(optimal_cutoff)
}
