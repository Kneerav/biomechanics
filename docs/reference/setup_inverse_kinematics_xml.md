# Setup Inverse Kinematics XML

This function sets up an XML file for inverse kinematics by modifying a
given base XML file with the specified parameters (e.g., model, marker
data, time range, etc.) and saves the updated XML file to a specified
filename.

## Usage

``` r
setup_inverse_kinematics_xml(
  base_file = "setup_ik_base.xml",
  results_directory = ".",
  model_file = "model_scaled.osim",
  constraint_weight = "Inf",
  accuracy = "1e-005",
  marker_file = "marker_data.trc",
  coordinate_file = "Unassigned",
  start_time,
  end_time,
  report_errors = "true",
  output_motion_file = "ik.mot",
  report_marker_locations = "true",
  Markers,
  Weights = rep(1, length(Markers)),
  setup_filename = "setup_ik.xml"
)
```

## Arguments

- base_file:

  A character string specifying the path to the base XML file. Default
  is "setup_ik_base.xml".

- results_directory:

  A character string specifying the directory for saving results.
  Default is the current directory (".").

- model_file:

  A character string specifying the path to the scaled model file.
  Default is "model_scaled.osim".

- constraint_weight:

  A character string or numeric specifying the constraint weight.
  Default is "Inf".

- accuracy:

  A character string or numeric specifying the desired accuracy. Default
  is "1e-005".

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

- report_errors:

  A character string specifying whether to report errors ("true" or
  "false"). Default is "true".

- output_motion_file:

  A character string specifying the output motion file name. Default is
  "ik.mot".

- report_marker_locations:

  A character string specifying whether to report marker locations
  ("true" or "false"). Default is "true".

- Markers:

  A character vector specifying the names of the markers used in the
  inverse kinematics setup.

- Weights:

  A numeric vector specifying the weight for each marker. Default is a
  vector of 1's with the same length as \`Markers\`.

- setup_filename:

  A character string specifying the name of the output setup XML file.
  Default is "setup_ik.xml".

## Value

The function does not return a value. It writes the modified XML to the
specified setup_filename.
