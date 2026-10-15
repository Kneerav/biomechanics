# Read TRC File into Data Frame

This function reads a TRC (marker trajectory) file and returns it as a
data frame. The function processes the header to extract marker names
and adjusts column names accordingly.

## Usage

``` r
read_trc(filename)
```

## Arguments

- filename:

  A character string specifying the path to the TRC file to be read.

## Value

A data frame with columns for frame number, time, and marker coordinates
(X, Y, Z).
