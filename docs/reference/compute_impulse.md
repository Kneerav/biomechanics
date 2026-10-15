# Compute Area Under the Curve (AUC) Using Trapezoidal Integration

This function calculates the area under the curve (AUC) using the
trapezoidal rule from the \`pracma\` package. It also supports computing
the positive AUC (sum of areas above the x-axis) and negative AUC (sum
of areas below the x-axis).

## Usage

``` r
compute_impulse(time, variable, type = "total")
```

## Arguments

- time:

  A numeric vector representing the x-coordinates of the data points,
  typically time in seconds.

- variable:

  A numeric vector representing the y-coordinates of the data points,
  typically a kinetic quantity like force or moment.

- type:

  A character string specifying the type of AUC to compute. Options are
  "total", "positive", or "negative". Default is "total".

## Value

A numeric value representing the computed AUC based on the specified
type.

## Examples

``` r
# Example time and variable data
time <- c(1, 2, 3, 4, 5)
variable <- c(-2, 3, 5, -7, 6)
# Compute total AUC
auc_total <- compute_impulse(time, variable, type = "total")
print(auc_total)
#> [1] 3
# Compute positive AUC
auc_positive <- compute_impulse(time, variable, type = "positive")
print(auc_positive)
#> [1] 11
# Compute negative AUC
auc_negative <- compute_impulse(time, variable, type = "negative")
print(auc_negative)
#> [1] -8
```
