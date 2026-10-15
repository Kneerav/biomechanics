# Compute Joint Angles from Rotation Frames

This function computes Euler angles from a series of 3x3 rotation
matrices.

## Usage

``` r
compute_joint_angles(rotation_frames)
```

## Arguments

- rotation_frames:

  A 3-dimensional array of size 3x3xN, where each 3x3 slice represents a
  rotation matrix.

## Value

A matrix of size N x 3, where each row contains the Euler angles (in
degrees) corresponding to a 3x3 rotation matrix.
