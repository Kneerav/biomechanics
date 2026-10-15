# Compute Corrected Cutoff Frequency for Filter

This function computes the corrected cutoff frequency for a lowpass or
highpass filter using the correction factor by David A. Winter. For
highpass filters, Robertson's high-pass modification is applied.

## Usage

``` r
compute_cutoff_correction(
  filter_passes = 2,
  cutoff_frequency = 20,
  sampling_frequency = 200,
  type = "lowpass"
)
```

## Arguments

- filter_passes:

  Integer. The number of passes of the filter. Default is 2.

- cutoff_frequency:

  Numeric. The cutoff frequency in Hz. Default is 6 for lowpass.

- sampling_frequency:

  Numeric. The sampling frequency in Hz. Default is 200.

- type:

  Character. The type of filter. Must be either "lowpass" or "highpass".
  Default is "lowpass".

## Value

Numeric. The corrected cutoff frequency in Hz.
