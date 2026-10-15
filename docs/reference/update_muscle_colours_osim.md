# Update OpenSim Muscle Colours

This function updates the muscle colours of an OpenSim model based on
the given inputs. It reads in an OpenSim model, updates the muscle
colours, and saves the model to a new file.

## Usage

``` r
update_muscle_colours_osim(
  model_input_file = "Baseline_markers.osim",
  model_output_file = "Baseline_markers_coloured.osim",
  colours = c("#009E73", "#009E73"),
  muscles = c("bflh140_r", "bfsh140_r")
)
```

## Arguments

- model_input_file:

  Path to the input OpenSim model (.osim file).

- model_output_file:

  Path to save the updated OpenSim model.

- colours:

  A vector of colour codes (in hex format) for the muscles to be
  updated.

- muscles:

  A vector of muscle names to be updated in the model.

## Value

A .osim file with updated muscle colours.
