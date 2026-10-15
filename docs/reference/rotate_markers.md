# Rotate 3D Marker Coordinates in a Data Frame

Applies a 3D rotation to all marker columns in a data frame. Marker
columns are expected to follow the naming pattern: \`marker_X\`,
\`marker_Y\`, \`marker_Z\`.

## Usage

``` r
rotate_markers(data, angle_deg, axis, ignore_case = T)
```

## Arguments

- data:

  A data frame containing 3D marker coordinates.

- angle_deg:

  Numeric. The rotation angle in degrees.

- axis:

  Character. The axis of rotation: "x", "y", or "z".

- ignore_case:

  Logical. Should column name matching of the suffix (e.g., "\_X")
  ignore case? Default is TRUE.

## Value

A data frame with rotated marker coordinates.
