# Setup Scale XML

This function sets up an XML file for scaling a model by modifying a
given base XML file with the specified parameters (e.g., mass, height,
age, etc.) and saves the updated XML file to a specified filename.

## Usage

``` r
setup_scale_xml(
  base_file = "setup_scale_base.xml",
  mass,
  height,
  age,
  model_file = "baseline_model.osim",
  marker_file = "marker_data.trc",
  coordinate_file = "Unassigned",
  start_time,
  end_time,
  output_motion_file = "ik.mot",
  output_model_file = "model_scaled.osim",
  output_marker_file = "marker_locations.sto",
  setup_filename = "setup_scale.xml"
)
```

## Arguments

- base_file:

  A character string specifying the path to the base XML file. Default
  is "setup_scale_base.xml".

- mass:

  A numeric value specifying the mass of the model.

- height:

  A numeric value specifying the height of the model.

- age:

  A numeric value specifying the age of the model.

- model_file:

  A character string specifying the path to the baseline model file.
  Default is "baseline_model.osim".

- marker_file:

  A character string specifying the path to the marker data file.
  Default is "marker_data.trc".

- coordinate_file:

  A character string specifying the path to the coordinate file. Default
  is "Unassigned".

- start_time:

  A numeric value specifying the start time of the simulation.

- end_time:

  A numeric value specifying the end time of the simulation.

- output_motion_file:

  A character string specifying the output motion file name. Default is
  "ik.mot".

- output_model_file:

  A character string specifying the output model file name. Default is
  "model_scaled.osim".

- output_marker_file:

  A character string specifying the output marker file name. Default is
  "marker_locations.sto".

- setup_filename:

  A character string specifying the name of the output setup XML file.
  Default is "setup_scale.xml".

## Value

The function does not return a value. It writes the modified XML to the
specified setup_filename.
