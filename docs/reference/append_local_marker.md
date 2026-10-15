# Append a Marker Defined in a Local Coordinate System to Global Data

Transforms a marker defined in a local reference frame (defined by 3
markers) into the global coordinate system for each row in the dataset,
and appends it to the data.

## Usage

``` r
append_local_marker(
  data,
  marker_local,
  frame_markers,
  local_marker_name,
  append = TRUE
)
```

## Arguments

- data:

  A data frame of marker data, with columns named in the format
  \`marker_X\`, \`marker_Y\`, \`marker_Z\`.

- marker_local:

  A numeric vector of length 3 representing the local coordinates (X,
  Y, Z) of the marker.

- frame_markers:

  A character vector of 3 marker names that define the local coordinate
  system. The second marker is used as the origin.

- local_marker_name:

  A string giving the name of the local marker to be added.

- append:

  Logical. If \`TRUE\` (default), appends the marker to the original
  data. If \`FALSE\`, returns only the transformed marker positions.

## Value

A data frame. If \`append = TRUE\`, returns the original \`data\` with
the new marker columns appended. If \`FALSE\`, returns a data frame with
just the transformed marker.
