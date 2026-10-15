# Scale Muscle Strength in OpenSim Model

This function scales the muscle strength of an OpenSim model by a
specified factor. It can save the modified model to a file or return the
updated model object.

## Usage

``` r
strength_scale_osim(
  model_input_file = "Baseline_model.osim",
  model_output_file = "Baseline_model_stronger.osim",
  write_file = TRUE,
  return_object = FALSE,
  scaleFactor = 5
)
```

## Arguments

- model_input_file:

  A string specifying the path to the input OpenSim model file (default
  is "Baseline_model.osim").

- model_output_file:

  A string specifying the path for the output updated OpenSim model file
  (default is "Baseline_model_stronger.osim").

- write_file:

  A logical value indicating whether to write the updated model to a
  file (default is TRUE).

- return_object:

  A logical value indicating whether to return the updated model object
  (default is FALSE).

- scaleFactor:

  A numeric value indicating the scaling factor for muscle strength
  (default is 5).

## Value

If \`return_object\` is TRUE, returns the modified OpenSim model object;
otherwise, NULL.
