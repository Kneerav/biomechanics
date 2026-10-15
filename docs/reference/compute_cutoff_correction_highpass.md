# Compute Corrected Cutoff Frequency for Highpass Filter

This function computes the corrected cutoff frequency for a highpass
filter using the correction factor by David A. Winter and Robertson's
high-pass modification.

## Usage

``` r
compute_cutoff_correction_highpass(
  filter_passes = 2,
  cutoff_frequency = 20,
  sampling_frequency = 200
)
```

## Arguments

- filter_passes:

  Integer. The number of passes of the filter. Default is 2.

- cutoff_frequency:

  Numeric. The cutoff frequency in Hz. Default is 20.

- sampling_frequency:

  Numeric. The sampling frequency in Hz. Default is 200.

## Value

Numeric. The corrected cutoff frequency in Hz.
