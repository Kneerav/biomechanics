% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_work.R
\name{compute_work}
\alias{compute_work}
\title{Compute Area Under the Curve (AUC) Using Trapezoidal Integration}
\usage{
compute_work(time, power, type = "total")
}
\arguments{
\item{time}{A numeric vector representing the x-coordinates of the data points, typically time in seconds.}

\item{power}{A numeric vector representing the y-coordinates of the data points, typically power in watts.}

\item{type}{A character string specifying the type of AUC to compute. Options are "total", "positive", or "negative". Default is "total".}
}
\value{
A numeric value representing the computed AUC based on the specified type.
}
\description{
This function calculates the area under the curve (AUC) using the trapezoidal rule from the `pracma` package.
It also supports computing the positive AUC (sum of areas above the x-axis) and negative AUC (sum of areas below the x-axis).
}
\examples{
# Example time and power data
time <- c(1, 2, 3, 4, 5)
power <- c(-2, 3, 5, -7, 6)
# Compute total AUC
auc_total <- compute_work(time, power, type = "total")
print(auc_total)
# Compute positive AUC
auc_positive <- compute_work(time, power, type = "positive")
print(auc_positive)
# Compute negative AUC
auc_negative <- compute_work(time, power, type = "negative")
print(auc_negative)
}
