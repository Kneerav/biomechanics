# Rigid Body Transformation (Söderkvist & Wedin Method) Between Two Marker Frames

Computes the 4x4 rigid body transformation matrix between two frames of
3D marker data, using the SVD method of Söderkvist & Wedin (1993).

## Usage

``` r
rigid_transform_svd(reference_data, target_data)
```

## Arguments

- reference_data:

  A data frame or matrix representing the reference frame (e.g., time
  1).

- target_data:

  A data frame or matrix representing the target frame (e.g., time 2).

## Value

A list containing:

- T:

  4x4 transformation matrix (rotation and translation)

- res:

  Residual error (Euclidean norm adjusted by DOF)

## References

Söderkvist & Wedin (1993). Determining the movements of the skeleton
using well-configured markers. Journal of Biomechanics, 26:1473–1477.
