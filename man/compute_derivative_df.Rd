% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_derivative_df.R
\name{compute_derivative_df}
\alias{compute_derivative_df}
\title{Compute Derivatives for Each Column in Data Frame}
\usage{
compute_derivative_df(
  data,
  time_col = 1,
  method = c("spline", "gradient"),
  deriv = 1
)
}
\arguments{
\item{data}{A data frame where the first column is the time values and other columns
represent the data (e.g., angular positions).}

\item{time_col}{A character or numeric value specifying the time column.
If numeric, it represents the column position. If character, it represents the column name.}

\item{method}{A character string specifying the method to compute the derivative.
Options are "spline" for smooth spline fitting or "gradient" for numerical gradient using \code{pracma::gradient}.
Default is "spline".}

\item{deriv}{An integer specifying the order of the derivative. Default is 1 for the first derivative.
Higher values (e.g., 2) will return higher derivatives (second derivative, etc.).}
}
\value{
A data frame with the same structure as the input, but with columns for angular velocities (or higher derivatives).
}
\description{
This function computes the angular velocity (first derivative of angular position)
for each column of data in a data frame, based on the specified time column.
The derivative can be computed using either a smooth spline fit or numerical gradient.
}
\examples{
# Example data frame
data <- data.frame(time = seq(0, 10, by = 0.1),
                   position1 = sin(seq(0, 10, by = 0.1)),
                   position2 = cos(seq(0, 10, by = 0.1)))

# Compute derivatives using spline method (time column by name)
angular_velocities_spline <- compute_derivative_dataframe(data, time_col = "time", method = "spline", deriv = 1)

# Compute derivatives using gradient method (time column by position)
angular_velocities_gradient <- compute_derivative_dataframe(data, time_col = 1, method = "gradient", deriv = 1)

}
