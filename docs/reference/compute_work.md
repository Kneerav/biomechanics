# Compute Area Under the Curve (AUC) Using Trapezoidal Integration

This function calculates the area under the curve (AUC) using the
trapezoidal rule from the \`pracma\` package. It also supports computing
the positive AUC (sum of areas above the x-axis) and negative AUC (sum
of areas below the x-axis).

## Usage

``` r
compute_work(time, power, type = "total")
```

## Arguments

- time:

  A numeric vector representing the x-coordinates of the data points,
  typically time in seconds.

- power:

  A numeric vector representing the y-coordinates of the data points,
  typically power in watts.

- type:

  A character string specifying the type of AUC to compute. Options are
  "total", "positive", or "negative". Default is "total".

## Value

A numeric value representing the computed AUC based on the specified
type.

## Examples

``` r
# Example time and power data
time <- c(1, 2, 3, 4, 5)
power <- c(-2, 3, 5, -7, 6)
# Compute total AUC
auc_total <- compute_work(time, power, type = "total")
print(auc_total)
#> [1] 3
# Compute positive AUC
auc_positive <- compute_work(time, power, type = "positive")
print(auc_positive)
#> [1] 11
# Compute negative AUC
auc_negative <- compute_work(time, power, type = "negative")
print(auc_negative)
#> [1] -8
```
