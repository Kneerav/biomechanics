#' Compute Derivatives for Each Column in Data Frame
#'
#' This function computes the angular velocity (first derivative of angular position)
#' for each column of data in a data frame, based on the specified time column.
#' The derivative can be computed using either a smooth spline fit or numerical gradient.
#'
#' @param data A data frame where the first column is the time values and other columns
#'   represent the data (e.g., angular positions).
#' @param time_col A character or numeric value specifying the time column.
#'   If numeric, it represents the column position. If character, it represents the column name.
#' @param method A character string specifying the method to compute the derivative.
#'   Options are "spline" for smooth spline fitting or "gradient" for numerical gradient using \code{pracma::gradient}.
#'   Default is "spline".
#' @param deriv An integer specifying the order of the derivative. Default is 1 for the first derivative.
#'   Higher values (e.g., 2) will return higher derivatives (second derivative, etc.).
#'
#' @return A data frame with the same structure as the input, but with columns for angular velocities (or higher derivatives).
#'
#' @examples
#' # Example data frame
#' data <- data.frame(time = seq(0, 10, by = 0.1),
#'                    position1 = sin(seq(0, 10, by = 0.1)),
#'                    position2 = cos(seq(0, 10, by = 0.1)))
#'
#' # Compute derivatives using spline method (time column by name)
#' angular_velocities_spline <- compute_derivative_dataframe(data, time_col = "time", method = "spline", deriv = 1)
#'
#' # Compute derivatives using gradient method (time column by position)
#' angular_velocities_gradient <- compute_derivative_dataframe(data, time_col = 1, method = "gradient", deriv = 1)
#'
#' @export
compute_derivative_df <- function(data, time_col = 1, method = c("spline", "gradient"), deriv = 1) {

  # Match method argument to ensure it's valid
  method <- match.arg(method)

  # Check if time_col is numeric or character and extract the correct time column
  if (is.numeric(time_col)) {

    time_i <- data[[time_col]]

  } else if (is.character(time_col)) {

    time_col <- which(colnames(data) == time_col)
    time_i <- data[[time_col]]

  } else {

    stop("time_col must be either a column name or column position.")

  }

  # Initialize a new data frame to store the angular velocities (or higher derivatives)
  y_deriv_df <- data.frame(time = time_i)

  # Loop over all columns except the time column
  for (col_name in colnames(data)) {

    if (col_name != colnames(data)[time_col]) {

      # Apply derivative function to each data column
      y_0 <- data[[col_name]]
      y_deriv <- compute_derivative(time_i, y_0, method, deriv)

      # Store the result in the new data frame
      y_deriv_df[[paste0("", col_name)]] <- y_deriv
    }
  }

  return(y_deriv_df)

}
