# Write TRC File from Data Frame

This function writes a data frame to a TRC (marker trajectory) file
format used in motion capture data.

## Usage

``` r
write_trc(data, unit = "mm", filename)
```

## Arguments

- data:

  A data frame containing marker data. The first two columns should be
  frame number and time, followed by marker data in X, Y, Z format.

- unit:

  A character string specifying the unit of measurement (e.g., "mm").
  Default is "mm".

- filename:

  A character string specifying the path to the file to be written.

## Examples

``` r
# Create a sample data frame with marker data
df <- data.frame(Frame = 1:10, Time = seq(0, 1, length.out = 10), 
                 Marker1_X = rnorm(10), Marker1_Y = rnorm(10), Marker1_Z = rnorm(10),
                 Marker2_X = rnorm(10), Marker2_Y = rnorm(10), Marker2_Z = rnorm(10))
# Write the data frame to a TRC file named "example.trc"
write_trc(df, unit = "mm", filename = "example.trc")
#> Warning: appending column names to file
#> Warning: appending column names to file
```
