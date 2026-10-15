# Generate Hex Colors from a Color Scale

This function maps numeric values within a specified range to a color
gradient defined by two colors. It returns the corresponding hex codes
and optionally plots the color scale.

## Usage

``` r
get_hex_colors(
  numeric_values,
  lower_bound = 0,
  upper_bound = 1,
  color_low = "orange",
  color_high = "green",
  resolution = 100,
  plot_scale = FALSE
)
```

## Arguments

- numeric_values:

  A numeric vector of values to map to the color scale.

- lower_bound:

  A numeric value specifying the lower bound of the numeric values
  (default: 0).

- upper_bound:

  A numeric value specifying the upper bound of the numeric values
  (default: 1).

- color_low:

  A string specifying the color at the low end of the gradient (default:
  "orange").

- color_high:

  A string specifying the color at the high end of the gradient
  (default: "green").

- resolution:

  The number of colors to generate in the color gradient (default: 100).

- plot_scale:

  A logical value indicating whether to plot the color scale (default:
  FALSE).

## Value

A vector of hex color codes corresponding to the input numeric values.

## Examples

``` r
# Example usage of get_hex_colors
numeric_values <- c(5, 10, 15)
get_hex_colors(numeric_values, lower_bound = 0, upper_bound = 20, color_low = "blue", color_high = "red")
#> [1] "#4000BE" "#80007E" "#BE0040"
```
