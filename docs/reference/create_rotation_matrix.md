# Create a 3D Rotation Matrix

Generates a 3x3 rotation matrix for rotating points in 3D space around
the x, y, or z axis by a specified angle. Note that the result are
standard active rotation matrices, meaning they rotate vectors within a
fixed frame using the right-hand rule.

## Usage

``` r
create_rotation_matrix(angle, axis, in_degrees = TRUE)
```

## Arguments

- angle:

  Numeric. The rotation angle.

- axis:

  Character. The axis of rotation: "x", "y", or "z".

- in_degrees:

  Logical. If TRUE (default), the input angle is assumed to be in
  degrees and is converted to radians. If FALSE, the angle is assumed to
  already be in radians.

## Value

A 3x3 numeric matrix representing the rotation.

## Examples

``` r
create_rotation_matrix(90, "z")                # degrees (default)
#>              [,1]          [,2] [,3]
#> [1,] 6.123032e-17 -1.000000e+00    0
#> [2,] 1.000000e+00  6.123032e-17    0
#> [3,] 0.000000e+00  0.000000e+00    1
create_rotation_matrix(pi/2, "z", FALSE)       # radians
#>              [,1]          [,2] [,3]
#> [1,] 6.123032e-17 -1.000000e+00    0
#> [2,] 1.000000e+00  6.123032e-17    0
#> [3,] 0.000000e+00  0.000000e+00    1
```
