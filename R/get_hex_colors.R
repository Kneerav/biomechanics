#' Generate Hex Colors from a Color Scale
#'
#' This function maps numeric values within a specified range to a color gradient defined by two colors.
#' It returns the corresponding hex codes and optionally plots the color scale.
#'
#' @param numeric_values A numeric vector of values to map to the color scale.
#' @param lower_bound A numeric value specifying the lower bound of the numeric values (default: 0).
#' @param upper_bound A numeric value specifying the upper bound of the numeric values (default: 1).
#' @param color_low A string specifying the color at the low end of the gradient (default: "orange").
#' @param color_high A string specifying the color at the high end of the gradient (default: "green").
#' @param resolution The number of colors to generate in the color gradient (default: 100).
#' @param plot_scale A logical value indicating whether to plot the color scale (default: FALSE).
#' 
#' @return A vector of hex color codes corresponding to the input numeric values.
#' @export
#'
#' @examples
#' # Example usage of get_hex_colors
#' numeric_values <- c(5, 10, 15)
#' get_hex_colors(numeric_values, lower_bound = 0, upper_bound = 20, color_low = "blue", color_high = "red")
get_hex_colors <- function(numeric_values, 
                           lower_bound = 0, 
                           upper_bound = 1,
                           color_low = "orange", 
                           color_high = "green", 
                           resolution = 100,
                           plot_scale = FALSE) {
  
  # Check if the upper_bound is greater than the lower_bound
  if (upper_bound <= lower_bound) {
    stop("upper_bound must be greater than lower_bound")
  }
  
  # Create the color ramp function based on the two specified colors
  color_scale <- colorRampPalette(c(color_low, color_high))
  
  # Generate the color gradient (a smooth scale between 0 and 1)
  color_gradient <- color_scale(resolution)
  
  # Scale the numeric values to the corresponding positions in the color ramp
  scaled_indices <- round((numeric_values - lower_bound) / (upper_bound - lower_bound) * (length(color_gradient) - 1)) + 1
  
  # Ensure indices stay within valid range
  scaled_indices <- pmin(pmax(scaled_indices, 1), length(color_gradient))
  
  # Return the selected hex codes
  selected_colors <- color_gradient[scaled_indices]
  
  # Plot the color scale if plot_scale is TRUE
  if (plot_scale) {
    # Use the basic plot function to display the color scale
    plot(rep(1, resolution), col = color_gradient, pch = 19, cex = 3, 
         xlab = "Scale", ylab = "", main = "Color Gradient", xaxt = "n", yaxt = "n")
  }
  
  # Return the selected hex color codes
  return(selected_colors)
}
