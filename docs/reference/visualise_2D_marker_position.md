# Visualize 2D Marker Data in Selected Plane

This function visualizes marker data in a 2D plot (sagittal, frontal, or
transverse plane) at a selected frame. It plots the markers as points on
the chosen plane and adjusts the axis limits according to the marker
data.

## Usage

``` r
visualise_2D_marker_position(data, selected_frame, plane, point_size = 2)
```

## Arguments

- data:

  A data frame containing the marker data (including columns for X, Y, Z
  positions).

- selected_frame:

  An integer specifying the frame number to visualize.

- plane:

  A character string specifying the plane to plot. Options are
  "sagittal", "frontal", or "transverse".

- point_size:

  A numeric value specifying the size of the points in the plot. Default
  is 2.

## Value

A ggplot2 object representing the 2D visualization of the marker data.
