# Time normalises continuous data

Takes a dataframe containing continuous data and resamples it to the
specified number of points

## Usage

``` r
Time_normalise()
```

## Arguments

- DF:

  a dataframe containing the continuous data that needs to be time
  normalised to specified number of points. The first column in the data
  frame must be the time or sample number column in ascending order

- nodes:

  numeric value which indicates how many points you would like your data
  to be resampled to

## Value

A dataframe of nrow(nodes) which contains the resampled data
