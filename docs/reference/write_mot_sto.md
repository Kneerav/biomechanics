# Write Data Frame to OpenSim MOT or STO File

This function writes a data frame to an OpenSim MOT or STO file with a
specific header format.

## Usage

``` r
write_mot_sto(data, name = "Coordinates", inDegrees = "no", filename)
```

## Arguments

- data:

  A data frame containing the data to be written. Each column represents
  a variable, where the first column should represent time..

- name:

  A character string specifying the name of the file. Default is
  "Coordinates".

- inDegrees:

  A character string specifying whether the data is in degrees. Default
  is "no". Use "yes" for degrees.

- filename:

  A character string specifying the name of the file to which the data
  should be written. Include the extension .mot or .sto for
  compatability with OpenSim.

## Examples

``` r
# Create a sample data frame
df <- data.frame(time = 1:10, value = sin(1:10))
# Write the data frame to a file named "example.mot"
write_mot_sto(df, name = "ExampleData", inDegrees = "no", filename = "example.mot")
#> Warning: appending column names to file
```
