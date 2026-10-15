# Update Maximum Contraction Velocity in an OpenSim Model

This function loads an OpenSim model, modifies the maximum contraction
velocity of all muscles, and optionally writes the modified model to a
file or returns it.

## Usage

``` r
update_max_contraction_velocity_osim(
  model_input_file = "Baseline_model.osim",
  model_output_file = "Baseline_model_stronger.osim",
  new_max = 20,
  write_file = TRUE,
  return_object = FALSE
)
```

## Arguments

- model_input_file:

  Path to the input \`.osim\` model file. Defaults to
  \`"Baseline_model.osim"\`.

- model_output_file:

  Path to the output \`.osim\` file. Defaults to
  \`"Baseline_model_stronger.osim"\`.

- new_max:

  Numeric value specifying the new maximum contraction velocity for all
  muscles. Defaults to \`20\`.

- write_file:

  Logical; whether to write the modified model to a file. Defaults to
  \`TRUE\`.

- return_object:

  Logical; whether to return the modified model object. Defaults to
  \`FALSE\`.

## Value

If \`return_object = TRUE\`, returns the modified OpenSim model object.
Otherwise, returns \`NULL\` invisibly.
