# Adjust Reserve Actuators to Align with Pelvis Center of Mass

This function modifies the point location of the first three reserve
actuators in an OpenSim force set file so they act through the center of
mass of the pelvis body.

## Usage

``` r
adjust_reserve_actuators_osim(
  model_file = "Model_Scaled.osim",
  actuators_file = "reserve_actuators.xml",
  write_file = FALSE,
  return_object = TRUE
)
```

## Arguments

- model_file:

  Character. Path to the OpenSim model file (\`.osim\`). Defaults to
  \`"Model_Scaled.osim"\`.

- actuators_file:

  Character. Path to the actuators XML file. Defaults to
  \`"reserve_actuators.xml"\`.

- write_file:

  Logical: If TRUE, writes the moment arm values to a file. Default is
  FALSE.

- return_object:

  Logical: If TRUE, returns the moment arm data as a data frame. Default
  is TRUE.

## Value

If \`return_object\` is TRUE, a data frame containing the computed
moment arm values. Otherwise, nothing is returned.
