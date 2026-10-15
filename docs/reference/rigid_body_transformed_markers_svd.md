# Apply Rigid Body Transformation to Markers Using SVD

This function applies a rigid body transformation to a set of target 3D
markers using singular value decomposition (SVD) based on a reference
set of 3D markers.

## Usage

``` r
rigid_body_transformed_markers_svd(reference_data, target_data)
```

## Arguments

- reference_data:

  A matrix or data frame containing the reference 3D marker data
  (dimensions should be N x 3M, where N is the number of frames and M is
  the number of markers).

- target_data:

  A matrix or data frame containing the target 3D marker data
  (dimensions should be N x 3M, where N is the number of frames and M is
  the number of markers).

## Value

A data frame containing the transformed 3D marker data for each frame.
