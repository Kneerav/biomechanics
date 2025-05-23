% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_hex_colors.R
\name{get_hex_colors}
\alias{get_hex_colors}
\title{Generate Hex Colors from a Color Scale}
\usage{
get_hex_colors(
  numeric_values,
  lower_bound = 0,
  upper_bound = 1,
  color_low = "orange",
  color_high = "green",
  resolution = 100,
  plot_scale = FALSE
)
}
\arguments{
\item{numeric_values}{A numeric vector of values to map to the color scale.}

\item{lower_bound}{A numeric value specifying the lower bound of the numeric values (default: 0).}

\item{upper_bound}{A numeric value specifying the upper bound of the numeric values (default: 1).}

\item{color_low}{A string specifying the color at the low end of the gradient (default: "orange").}

\item{color_high}{A string specifying the color at the high end of the gradient (default: "green").}

\item{resolution}{The number of colors to generate in the color gradient (default: 100).}

\item{plot_scale}{A logical value indicating whether to plot the color scale (default: FALSE).}
}
\value{
A vector of hex color codes corresponding to the input numeric values.
}
\description{
This function maps numeric values within a specified range to a color gradient defined by two colors.
It returns the corresponding hex codes and optionally plots the color scale.
}
\examples{
# Example usage of get_hex_colors
numeric_values <- c(5, 10, 15)
get_hex_colors(numeric_values, lower_bound = 0, upper_bound = 20, color_low = "blue", color_high = "red")
}
