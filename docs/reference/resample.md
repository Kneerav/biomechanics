# Resample a numeric data frame (where the first column represents time) to the specified sample rate

This function resamples a numeric data frame to the specified sample
rate using interpolation.

## Usage

``` r
resample(data, sample_rate_output = 100, method = "cubic")
```

## Arguments

- data:

  A numeric data frame where each column is a time series to be
  resampled. The first column should be a time vector.

- sample_rate_output:

  Integer, the desired sample rate (Hz) of the output signal. Default is
  100Hz.

- method:

  Character, the interpolation method to use. Default is "cubic".
  Options include "linear", "nearest", "spline", "pchip", etc.

## Value

A data frame with the same number of columns as \`data\`, but with each
column resampled to the specified sample rate.
