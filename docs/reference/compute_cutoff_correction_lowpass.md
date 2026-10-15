# Compute Corrected Cutoff Frequency for Lowpass Filter

This function computes the corrected cutoff frequency for a lowpass
filter using the correction factor by David A. Winter.

## Usage

``` r
compute_cutoff_correction_lowpass(
  filter_passes = 2,
  cutoff_frequency = 6,
  sampling_frequency = 200
)
```

## Arguments

- filter_passes:

  Integer. The number of passes of the filter. Default is 2.

- cutoff_frequency:

  Numeric. The cutoff frequency in Hz. Default is 6.

- sampling_frequency:

  Numeric. The sampling frequency in Hz. Default is 200.

## Value

Numeric. The corrected cutoff frequency in Hz.
