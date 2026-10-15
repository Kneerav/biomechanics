# Compute Rotation Between Frames

This function calculates the rotation matrix between corresponding
frames in two 3D arrays. Note that this function assumes the input
frames represent a global to local transformation.

## Usage

``` r
compute_rotation_between_frames(parent, child)
```

## Arguments

- parent:

  A 3-dimensional array where each slice represents a frame of rotation
  matrices.

- child:

  A 3-dimensional array of the same dimensions as \`parent\`,
  representing another set of rotation matrices.

## Value

A 3-dimensional array of the same dimensions as \`parent\` and
\`child\`, where each slice represents the rotation matrix between
corresponding frames.
