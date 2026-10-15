# Write GRF Data Frame to File

This function writes a GRF (Ground Reaction Force) data frame to a file
with a specific header format suitable for use in OpenSim.

## Usage

``` r
write_force_mot(data, filename)
```

## Arguments

- data:

  A data frame containing GRF data. The first column should represent
  time.

- filename:

  A character string specifying the name of the file to which the data
  should be written. Include the extension .mot for compatability with
  OpenSim.

## Examples

``` r
# Create a sample GRF data frame
df <- data.frame(time = 1:10, force = rnorm(10))
# Write the data frame to a file named "grf_data.mot"
write_force_mot(df, "grf_data.mot")
#> Warning: appending column names to file
```
