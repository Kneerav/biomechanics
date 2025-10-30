#' Plot RMSE vs. Cutoff Frequency for Residual Analysis
#'
#' This function performs a residual analysis by filtering a signal
#' across a range of cutoff frequencies and computing the root mean square error (RMSE)
#' between the raw and filtered signals. The results are plotted as RMSE vs. cutoff frequency.
#'
#' @param data Numeric vector. The raw signal data to be filtered.
#' @param sampling_rate Numeric. Sampling frequency of the data (Hz).
#' @param order Integer. Order of the Butterworth filter.
#' @param cutoff_range Numeric vector. Range of cutoff frequencies (Hz) to evaluate.
#'
#' @details
#' Residual analysis is used to determine an appropriate cutoff frequency
#' for filtering biomechanical data. The RMSE between raw and filtered data
#' is computed for each cutoff, and plotted to visualize when residual noise
#' becomes negligible.
#'
#' @references
#' Winter, David A. Biomechanics and motor control of human movement. John Wiley & Sons, 2009.
#'
#' @return A list containing:
#' \item{rmses}{Numeric vector of RMSE values for each cutoff frequency.}
#' \item{cutoff_range}{Numeric vector of cutoff frequencies used.}
#'
#' @importFrom ggplot2 ggplot geom_point aes theme_classic xlab ylab
#' @export
residual_analysis_curve <- function(data, sampling_rate, order, cutoff_range) {
  rmses <- c()
  filtered_signals <- list()

  # Loop through cutoff range to compute RMSE for each filtered signal
  for (cutoff in cutoff_range) {
    filtered_data <- butter_lowpass_filter(data, cutoff, sampling_rate, order)
    rmse <- biomechanics::compute_rmse(data, filtered_data)
    rmses <- c(rmses, rmse)
    filtered_signals[[length(filtered_signals) + 1]] <- filtered_data
  }

  # Convert to a numeric vector
  rmses <- unlist(rmses)

  # Plot RMSE vs. cutoff frequency
  g <- ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(cutoff_range, rmses), pch=21, fill="#024950")+
    ggplot2::theme_classic()+
    ggplot2::ylab("RMSE")+
    ggplot2::xlab("Cutoff frequency (Hz)")

  plot(g)

  # Return the RMS errors and the cutoff range for further processing
  return(list(rmses = rmses, cutoff_range = cutoff_range))
}
