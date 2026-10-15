# Analyse Frequency Content of a Data Frame

Computes the frequency spectrum of numerical columns in a data frame
using the Fast Fourier Transform (via \`seewave::spec()\`).

## Usage

``` r
analyse_frequency_content(data, exclude_cols = c(1, 2), sample_rate = 200)
```

## Arguments

- data:

  A data frame containing the time-domain signal data.

- exclude_cols:

  Integer vector indicating which columns to exclude from analysis
  (e.g., time or label columns).

- sample_rate:

  Numeric. The sampling rate of the signals in Hz (default is 200 Hz).

## Value

A data frame with frequency content for each included column:

- Frequencies_kHz:

  Frequencies in kilohertz.

- Magnitude:

  Magnitude of the frequency component.

- Frequencies:

  Frequencies in Hz.

- id:

  Column name from original data that this spectrum corresponds to.
