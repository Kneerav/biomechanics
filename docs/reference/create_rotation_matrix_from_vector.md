# Convert Rotation Vector to Rotation Matrix

Converts a 3-element rotation vector (axis-angle representation) into a
3x3 rotation matrix, using Rodrigues' rotation formula. Equivalent to
Python's `scipy.spatial.transform.Rotation.from_rotvec`.

## Usage

``` r
create_rotation_matrix_from_vector(theta3, in_degrees = FALSE)
```

## Arguments

- theta3:

  A numeric vector of length 3, representing the rotation vector. Its
  direction specifies the axis of rotation, and its magnitude specifies
  the angle (in radians by default).

- in_degrees:

  Logical. If `TRUE`, the elements of `theta3` are interpreted as
  degrees and converted to radians internally. Default is `FALSE`.

## Value

A 3x3 numeric matrix representing the rotation.

## References

<https://en.wikipedia.org/wiki/Axis-angle_representation#Rotation_vector>
<https://docs.scipy.org/doc/scipy/reference/generated/scipy.spatial.transform.Rotation.from_rotvec.html>

## Examples

``` r
create_rotation_matrix_from_vector(c(0.5, 0.4, 0.3))
#>            [,1]       [,2]       [,3]
#> [1,]  0.8801223 -0.1797154  0.4394168
#> [2,]  0.3715198  0.8369663 -0.4018214
#> [3,] -0.2955635  0.5169040  0.8034006
create_rotation_matrix_from_vector(c(30, 0, 0), in_degrees = TRUE)
#>      [,1]      [,2]       [,3]
#> [1,]    1 0.0000000  0.0000000
#> [2,]    0 0.8660254 -0.5000000
#> [3,]    0 0.5000000  0.8660254
```
