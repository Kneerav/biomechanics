# Compute a Reference Frame from Three Non-Colinear Markers

Constructs a 4x4 transformation matrix (local to global) representing a
reference frame defined by three non-colinear 3D points. The frame is
based on: - Origin: marker_2 - X-axis: from marker_2 to marker_3 -
Y-axis: from marker_2 to marker_1 - Z-axis: orthogonal to the XY-plane
(via cross product) Note that x is re-computed via cross product of y
and z to make the frame orthogonal

## Usage

``` r
compute_frame_generic(marker_1, marker_2, marker_3)
```

## Arguments

- marker_1:

  A numeric vector of length 3, representing a 3D point.

- marker_2:

  A numeric vector of length 3, representing a 3D point (used as
  origin).

- marker_3:

  A numeric vector of length 3, representing a 3D point.

## Value

A 4x4 numeric matrix representing a homogeneous transformation matrix.

## Examples

``` r
compute_frame_generic(c(1, 0, 0), c(0, 0, 0), c(0, 1, 0))
#>      x_orth y  z o
#> [1,]      0 1  0 0
#> [2,]      1 0  0 0
#> [3,]      0 0 -1 0
#> [4,]      0 0  0 1
```
