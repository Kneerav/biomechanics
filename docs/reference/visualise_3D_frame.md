# Add a 3D Reference Frame to a Plotly Object

Adds visual reference axes (X, Y, Z) to an existing 3D \`plotly\`
object. Each axis is drawn from the specified origin using direction
vectors from a 3x3 reference frame matrix.

## Usage

``` r
visualise_3D_frame(p, reference_frame, origin, scale_factor = 100)
```

## Arguments

- p:

  A \`plotly\` object (typically created with
  \`visualise_3D_marker_position()\`).

- reference_frame:

  A 3x3 numeric matrix. Each column represents a unit direction vector
  for an axis (X, Y, Z).

- origin:

  A numeric vector of length 3 specifying the origin point of the
  reference frame.

- scale_factor:

  Numeric value indicating the length of the axes to draw (default is
  100).

## Value

A modified \`plotly\` object with reference frame axes added.
