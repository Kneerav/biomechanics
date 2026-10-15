# Compute frame angular velocity from rotation matrices

Computes angular velocity from a time series of rotation matrices using
finite differences. Central differences are used for interior frames,
and either first- or second-order schemes are used at the endpoints.

## Usage

``` r
compute_frame_angular_velocity(
  frame_array,
  dt,
  endpoints = c("first", "second"),
  enforce_skew = TRUE
)
```

## Arguments

- frame_array:

  A 3 x 3 x N numeric array of rotation matrices. Each slice
  `frame_array[,,i]` must be a valid rotation matrix representing the
  orientation of the segment in the global frame.

- dt:

  Numeric scalar. Time step between frames (seconds).

- endpoints:

  Character string specifying endpoint method:

  "first"

  :   First-order forward/backward difference (default)

  "second"

  :   Second-order forward/backward difference

- enforce_skew:

  Logical. If `TRUE`, forces the angular velocity matrix to be
  skew-symmetric before extracting the vector. Recommended when working
  with noisy data.

## Value

A list with:

- `frame_velocity`3 x 3 x N array of angular velocity matrices

- `omega`N x 3 matrix of angular velocity vectors (global frame)

## Details

Angular velocity is computed as: \$\$\Omega = \dot{R} R^T\$\$ where
\\R\\ is the rotation matrix. The angular velocity vector is extracted
from the skew-symmetric matrix \\\Omega\\.

All inputs and outputs are expressed in the global (inertial) frame.
