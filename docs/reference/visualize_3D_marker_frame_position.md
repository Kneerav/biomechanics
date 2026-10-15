# Visualize 3D Marker Frame Position

This function visualizes the 3D positions of markers at a specified time
point from a dataset. It can also optionally display a reference frame.

## Usage

``` r
visualize_3D_marker_frame_position(
  data,
  selected_frame,
  reference_frame,
  origin
)
```

## Arguments

- data:

  A data frame containing marker data, including columns with '\_X',
  '\_Y', and '\_Z' suffixes for marker positions.

- selected_frame:

  A numeric value representing the time frame at which to visualize the
  markers.

- reference_frame:

  A 3x3 matrix representing a reference frame (optional). Each column
  should represent an axis of the reference frame.

- origin:

  A 1x3 numeric vector representing the origin point of the reference
  frame (optional).

## Value

A Plotly 3D scatter plot visualizing the marker positions at the
selected time point, with optional reference frame axes.
