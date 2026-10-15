# Visualize 3D Marker Positions and Segments

This function visualizes the 3D positions of markers at a specific
frame, connecting markers belonging to the same body segment with lines
to represent the segments of the body in a 3D space.

## Usage

``` r
visualise_3D_segment_position(
  data,
  selected_frame,
  mapping_data,
  marker_col,
  segment
)
```

## Arguments

- data:

  A data frame containing time and marker positions with columns
  formatted as \`Frame\`, \`Time\`, \`markername_X\`, \`markername_Y\`,
  \`markername_Z\`.

- selected_frame:

  Numeric value indicating the frame to visualize the marker positions.

- mapping_data:

  A data frame containing mapping information, with columns for marker
  names and associated body segments.

- marker_col:

  Name of the column in \`mapping_data\` that contains the marker names.

- segment:

  Name of the column in \`mapping_data\` that contains the body
  segments.

## Value

A \`plotly\` object representing the 3D scatter plot of marker positions
with segment lines.
