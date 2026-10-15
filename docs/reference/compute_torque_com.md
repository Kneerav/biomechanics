# Compute Torque at Center of Mass (COM)

This function calculates the torque at the center of mass (COM) given
the position of the COM, the ground reaction force (GRF), and the center
of pressure (COP). The torque is computed as the cross product of the
vector from the COM to the COP and the GRF vector.Note that positive
means counterclockwise rotation about a given axis (when the axis is
pointing to you).

## Usage

``` r
compute_torque_com(com_pos_vec, grf_vec, cop_vec)
```

## Arguments

- com_pos_vec:

  A numeric vector of length 3 representing the position of the center
  of mass (COM) in 3D space (x, y, z).

- grf_vec:

  A numeric vector of length 3 representing the ground reaction force
  (GRF) in 3D space (x, y, z).

- cop_vec:

  A numeric vector of length 3 representing the position of the center
  of pressure (COP) in 3D space (x, y, z).

## Value

A numeric vector of length 3 representing the torque (tx, ty, tz) about
the COM in 3D space.
