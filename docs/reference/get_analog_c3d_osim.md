# Get Analog Data from C3D File

This function reads a C3D file and extracts the analog data, optionally
writing it to a .sto file and/or returning the opensim data table as an
object.

## Usage

``` r
get_analog_c3d_osim(
  file_input,
  file_output = "Analog.sto",
  write_file = TRUE,
  return_object = FALSE
)
```

## Arguments

- file_input:

  A string specifying the path to the input C3D file.

- file_output:

  A string specifying the path for the output .sto file (default is
  "Analog.sto").

- write_file:

  A logical value indicating whether to write the output to a file
  (default is TRUE).

- return_object:

  A logical value indicating whether to return the analog data object
  (default is FALSE).

## Value

If \`return_object\` is TRUE, returns the analog data object; otherwise,
returns NULL.
