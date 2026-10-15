# Analyse joint power

This function computes joint power using inverse dynamics and joint
velocities from OpenSim. This function has the option to sum components
for a given joint and limb (e.g., hip, knee) will create a new column
for each joint (e.g., hip_r, hip_l).. By default, this summing excludes
any columns that contain the word "beta" (i.e., from the couple
patellofemoral joint).

## Usage

``` r
analyse_joint_power(
  inverse_dynamics_df,
  joint_vel_df,
  file_out = "joint_power.sto",
  in_degrees = TRUE,
  sum_joints = c("hip", "arm"),
  sum_suffix = c("_l", "_r"),
  write_file = TRUE,
  return_object = FALSE
)
```

## Arguments

- inverse_dynamics_df:

  A dataframe containing the inverse dynamics data (e.g., from an
  "inverse_dynamics.sto" file).

- joint_vel_df:

  A dataframe containing the joint velocities (e.g., from an
  "Model_scaled_Kinematics_u.sto" file).

- file_out:

  A string specifying the output file path for the joint power results
  (e.g., "joint_power.sto").

- in_degrees:

  A logical indicating whether the input joint velocities are in degrees
  (default is TRUE). If TRUE, velocities are converted to radians.

- sum_joints:

  A character vector of joints to sum (e.g., c("hip", "arm")). Default
  is c("hip", "arm").

- sum_suffix:

  A character vector of suffixes for left and right sides (e.g.,
  c("\_l", "\_r")). Default is c("\_l", "\_r").

- write_file:

  A logical indicating whether to write the results to a file (default
  is TRUE).

- return_object:

  A logical indicating whether to return the resulting data frame
  (default is FALSE).

## Value

If return_object = TRUE, returns a data frame with the computed joint
power. If write_file = TRUE, writes the data to a file.
