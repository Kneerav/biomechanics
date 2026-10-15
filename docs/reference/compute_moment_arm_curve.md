# Compute Moment Arm Curve for a Muscle-Coordinate Pair in an OpenSim Model

This function computes the moment arm curve for a specified muscle and
coordinate in an OpenSim model, iterating over the coordinate's range.
The moment arm is calculated at increments of a specified angle and can
be written to a file or returned as a data frame.

## Usage

``` r
compute_moment_arm_curve(
  model_input_file = "../TestData/Model_SCALED.osim",
  coordinate = "knee_angle_r",
  muscle = "vasmed_r",
  file_output = "vasmed_r_knee_angle_r_moment_arm.sto",
  deg_increment = 10,
  write_file = FALSE,
  return_object = TRUE
)
```

## Arguments

- model_input_file:

  A string representing the path to the OpenSim model file. Default is
  "../TestData/Model_SCALED.osim".

- coordinate:

  A string indicating the name of the coordinate of interest. Default is
  "knee_angle_r".

- muscle:

  A string indicating the name of the muscle of interest. Default is
  "vasmed_r".

- file_output:

  A string representing the file path to write the moment arm data.
  Default is "vasmed_r_knee_angle_r_moment_arm.sto".

- deg_increment:

  The angular increment (in degrees) for which the moment arm will be
  computed. Default is 10.

- write_file:

  Logical: If TRUE, writes the moment arm values to a file. Default is
  FALSE.

- return_object:

  Logical: If TRUE, returns the moment arm data as a data frame. Default
  is TRUE.

## Value

If \`return_object\` is TRUE, a data frame containing the computed
moment arm values. Otherwise, nothing is returned.
