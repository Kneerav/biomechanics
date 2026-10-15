# Compute Root Mean Square Error (RMSE) with Normalization Options (untested)

This function calculates the Root Mean Square Error (RMSE) between two
numeric vectors (signals) and offers options for normalizing the RMSE.

## Usage

``` r
compute_rmse(observed, predicted, normalise = "none")
```

## Arguments

- observed:

  A numeric vector of observed values (true values).

- predicted:

  A numeric vector of predicted values (model values).

- normalise:

  A character string specifying the normalization method. Options are
  "none", "max", "min", "range", "median", "mean" or "sd". Default is
  "none".

## Value

A numeric value representing the normalised RMSE between the observed
and predicted values.

## Examples

``` r
# Example observed and predicted signals
observed <- c(1.0, 2.0, 3.0, 4.0, 5.0)
predicted <- c(1.1, 2.1, 2.9, 4.2, 5.0)
# Compute RMSE without normalization
rmse_no_norm <- compute_rmse(observed, predicted, normalise = "none")
print(rmse_no_norm)
#> [1] 0.1183216
# Compute RMSE normalised by the max value of the observed signal
rmse_max <- compute_rmse(observed, predicted, normalise = "max")
print(rmse_max)
#> [1] 0.02366432
```
