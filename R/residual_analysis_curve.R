#' Plot RMSE vs. Cutoff Frequency for Residual Analysis
#'
#' This function performs a residual analysis by filtering a signal
#' across a range of cutoff frequencies and computing the root mean square error (RMSE)
#' between the raw and filtered signals. The results are plotted as RMSE vs. cutoff frequency.
#'
#' @param data Numeric vector or matrix. The vector (or columns of matrix) contains the raw signal data to be filtered.
#' @param sampling_rate Numeric. Sampling frequency of the data (Hz).
#' @param order Integer. Order of the Butterworth filter.
#' @param cutoff_vec Numeric vector. Vector of cutoff frequencies (Hz) to evaluate.
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
#' \item{cutoff_vec}{Numeric vector of cutoff frequencies used.}
#'
#' @importFrom ggplot2 ggplot geom_point aes theme_classic xlab ylab
#' @importFrom signal butter filtfilt
#' @export
residual_analysis_curve <- function(data, sampling_rate, order, cutoff_vec = seq(1,20,0.1)) {
  rmses <- numeric(length(cutoff_vec))
  filtered_signals <- vector("list", length(cutoff_vec))

  # Ensure data is a matrix
  data <- as.matrix(data)

  #function to filter
  butter_lowpass_filter <- function(data, cutoff, sampling_rate, order) {
    nyquist_freq <- 0.5 * sampling_rate
    normal_cutoff <- cutoff / nyquist_freq
    filter_coeffs <- signal::butter(order, normal_cutoff, type = "low", plane = "z")
    filtered_data <- signal::filtfilt(filter_coeffs, data)
    return(filtered_data)
  }

  for (i in seq_along(cutoff_vec)) {
    cutoff <- cutoff_vec[i]

    # Filter each column
    filtered_data <- apply(data, 2, function(col)
      butter_lowpass_filter(col, cutoff, sampling_rate, order)
    )

    # # Compute RMSE for each column and sum them
    # rmse_per_col <- apply(filtered_data, 2, function(col)
    #   biomechanics::compute_rmse(data[, which(filtered_data[1, ] == col[1])], col)
    # )

    #Better (and safer): just loop to compute RMSE properly
    rmse_per_col <- numeric(ncol(data))
    for (j in seq_len(ncol(data))) {
      rmse_per_col[j] <- biomechanics::compute_rmse(data[, j], filtered_data[, j])
    }

    rmses[i] <- sum(rmse_per_col)
    filtered_signals[[i]] <- filtered_data
  }

  # Plot RMSE vs. cutoff frequency
  g <- ggplot2::ggplot(data.frame(cutoff = cutoff_vec, rmse = rmses)) +
    ggplot2::geom_point(ggplot2::aes(x = cutoff, y = rmse), pch = 21, fill = "#024950") +
    ggplot2::theme_classic() +
    ggplot2::ylab("Summed RMSE") +
    ggplot2::xlab("Cutoff frequency (Hz)")

  print(g)

  # Return results
  return(list(rmses = rmses, cutoff_vec = cutoff_vec))
}

