# Analyse Angular Momentum for a Model

This function computes the segmental angular momentum about the centre
of mass of a body model based on position and velocity kinematics over
time, returning the results or saving them to a file.

## Usage

``` r
analyse_angular_momentum_com(
  model_file = "Model_SCALED.osim",
  body_pos_df = pos_df,
  body_vel_df = vel_df,
  file_output = "angular_momentum.sto",
  in_degrees = TRUE,
  write_file = TRUE,
  return_object = FALSE
)
```

## Arguments

- model_file:

  A string specifying the model file (in .osim format).

- body_pos_df:

  A data.frame containing the body position data (usually read from .sto
  format).

- body_vel_df:

  A data.frame containing the body velocity data (usually read from .sto
  format).

- file_output:

  A string specifying the output file for the angular momentum results.

- in_degrees:

  Logical; if TRUE, input angles and velocities are assumed to be in
  degrees. Defaults to TRUE.

- write_file:

  Logical; if TRUE, the function writes results to a file. Defaults to
  TRUE.

- return_object:

  Logical; if TRUE, the function returns the computed angular momentum
  as a data frame.

## Value

If return_object = TRUE, a data frame containing the angular momentum
values for each body at each time step.
