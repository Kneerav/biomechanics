# Extract Marker Set Information from OpenSim marker set file

This function extracts information about the markers in an OpenSim
marker set file, including their name, parent body, fixed status, and
location. Optionally, the results can be written to a CSV file.

## Usage

``` r
extract_markerset_info(
  marker_file = "markerset.xml",
  output_file = "marker_info.csv",
  write_to_file = FALSE,
  return_obj = TRUE
)
```

## Arguments

- marker_file:

  A character string specifying the path to the marker set XML file.

- output_file:

  A character string specifying the path to the output CSV file.

- write_to_file:

  A logical indicating whether to write the result to a CSV file.
  Default is \`FALSE\`.

- return_obj:

  A logical indicating whether to return the data frame as an object.
  Default is \`TRUE\`.

## Value

A data frame containing the marker information.
