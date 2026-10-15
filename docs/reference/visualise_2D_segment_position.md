# Visualize 2D Marker Data with Segments

This function visualizes marker data in a 2D plot (sagittal, frontal, or
transverse plane) and draws lines between markers that belong to the
same segment. The plot also supports adding a legend for the segments.

## Usage

``` r
visualise_2D_segment_position(
  data,
  selected_frame,
  mapping_data,
  marker_col,
  segment,
  plane = "sagittal",
  point_size = 2,
  show_legend = TRUE
)
```

## Arguments

- data:

  A data frame containing the marker data (including columns for X, Y, Z
  positions).

- selected_frame:

  An integer specifying the frame number to visualize.

- mapping_data:

  A data frame that maps each marker to a segment.

- marker_col:

  The column in \`mapping_data\` that contains the marker names.

- segment:

  The name of the column in \`mapping_data\` that indicates the segment
  to which each marker belongs.

- plane:

  A character string specifying the plane to plot. Options are
  "sagittal", "frontal", or "transverse". Default is "sagittal".

- point_size:

  A numeric value specifying the size of the points in the plot. Default
  is 2.

- show_legend:

  A logical indicating whether to show the legend. Default is TRUE.

## Value

A ggplot2 object representing the 2D visualization of the marker data
and segments.
