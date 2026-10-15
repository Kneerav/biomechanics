# Rotate 3D Coordinates Using a Rotation Matrix

Applies a 3x3 rotation matrix to a set of 3D coordinates.

## Usage

``` r
rotate_coords(x, y, z, rotation_matrix)
```

## Arguments

- x, :

  y, z Numeric vectors of equal length representing 3D coordinates.

- rotation_matrix:

  A 3x3 numeric matrix used to rotate the coordinates.

## Value

A data frame with rotated coordinates (\`x\`, \`y\`, \`z\`).

## Examples

``` r
rot_mat <- create_rotation_matrix(90, "z")
rotate_coords(1, 0, 0, rot_mat)
#>              x  y z
#> 1 6.123032e-17 -1 0
```
