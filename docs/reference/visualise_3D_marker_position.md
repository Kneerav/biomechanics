# Visualize 3D Marker Position

Visualizes the 3D positions of markers at a specific time point using
interactive 3D plots.

## Usage

``` r
visualise_3D_marker_position(data, selected_frame)
```

## Arguments

- data:

  A dataframe containing time and marker positions with columns
  formatted as \`Time\`, \`markername_X\`, \`markername_Y\`,
  \`markername_Z\`.

- selected_frame:

  Numeric value indicating the frame visualize the marker positions.

## Value

A \`plotly\` object representing the 3D scatter plot of marker
positions.
