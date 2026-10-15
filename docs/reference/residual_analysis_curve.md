# Plot RMSE vs. Cutoff Frequency for Residual Analysis

This function performs a residual analysis by filtering a signal across
a range of cutoff frequencies and computing the root mean square error
(RMSE) between the raw and filtered signals. The results are plotted as
RMSE vs. cutoff frequency.

## Usage

``` r
residual_analysis_curve(
  data,
  sampling_rate,
  order,
  cutoff_vec = seq(1, 20, 0.1)
)
```

## Arguments

- data:

  Numeric vector or matrix. The vector (or columns of matrix) contains
  the raw signal data to be filtered.

- sampling_rate:

  Numeric. Sampling frequency of the data (Hz).

- order:

  Integer. Order of the Butterworth filter.

- cutoff_vec:

  Numeric vector. Vector of cutoff frequencies (Hz) to evaluate.

## Value

A list containing:

- rmses:

  Numeric vector of RMSE values for each cutoff frequency.

- cutoff_vec:

  Numeric vector of cutoff frequencies used.

## Details

Residual analysis is used to determine an appropriate cutoff frequency
for filtering biomechanical data. The RMSE between raw and filtered data
is computed for each cutoff, and plotted to visualize when residual
noise becomes negligible.

## References

Winter, David A. Biomechanics and motor control of human movement. John
Wiley & Sons, 2009.
