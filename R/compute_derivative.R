#' Compute Derivative of Data
#'
#' This function computes the derivative of data (e.g., angular position) using either
#' a smooth spline fit or numerical gradient.
#'
#' @param x A numeric vector representing the independent variable (e.g., time).
#' @param y A numeric vector representing the dependent variable (e.g., angular position).
#' @param method A character string specifying the method to compute the derivative.
#'   Options are "spline" for smooth spline fitting or "gradient" for numerical gradient using \code{pracma::gradient}.
#'   Default is "spline".
#' @param deriv An integer specifying the order of the derivative. Default is 1 for the first derivative.
#'   Higher values (e.g., 2) will return higher derivatives (second derivative, etc.).
#'
#' @return A numeric vector representing the derivative (e.g., angular velocity or acceleration).
#'
#' @export
compute_derivative <- function(x, y, method = c("spline", "gradient"), deriv = 1) {

  # Match method argument to ensure it's valid
  method <- match.arg(method)

  if (method == "spline") {

    # Fit smooth spline
    y_0_spline <- smooth.spline(x, y)

    # Take the first derivative
    y_deriv_spline <- predict(y_0_spline, deriv = deriv)$y

  } else if (method == "gradient") {

    # Compute numerical gradient using pracma::gradient
    y_deriv_spline <- pracma::gradient(y, x)
  }

  return(y_deriv_spline)
}
