# Update Muscle Strength in OpenSim Model Using Handsfield Method

This function updates the muscle strength of a scaled OpenSim model
based on a reference model using the Handsfield scaling method. It
calculates new muscle properties and can save the modified model to a
file or return the updated model object.

## Usage

``` r
strength_update_handsfield_osim(
  model_scaled_file = "Baseline_scaled.osim",
  model_ref_file = "Baseline_markers.osim",
  model_output_file = "Model_updated.osim",
  write_file = TRUE,
  return_object = FALSE,
  scale_factor
)
```

## Arguments

- model_scaled_file:

  A string specifying the path to the scaled OpenSim model file (default
  is "Baseline_scaled.osim").

- model_ref_file:

  A string specifying the path to the reference OpenSim model file
  (default is "Baseline_markers.osim").

- model_output_file:

  A string specifying the path for the output updated OpenSim model file
  (default is "Model_updated.osim").

- write_file:

  A logical value indicating whether to write the updated model to a
  file (default is TRUE).

- return_object:

  A logical value indicating whether to return the updated model object
  (default is FALSE).

- scale_factor:

  A numeric value indicating the scaling factor for muscle volume.

## Value

If \`return_object\` is TRUE, returns the modified OpenSim model object;
otherwise, NULL.

## References

Handsfield GG, Meyer CH, Hart JM, Abel MF, Blemker SS. Relationships of
35 lower limb muscles to height and body mass quantified using MRI. J
Biomech. 2014 Feb 7;47(3):631-8. doi: 10.1016/j.jbiomech.2013.12.002.
