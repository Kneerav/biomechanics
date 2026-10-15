# Set Up Body Kinematics XML for Analysis

This function modifies an XML configuration file for body kinematics
analysis by updating various parameters related to the analysis setup.
The XML file is updated with information such as the model file, force
set files, results directory, analysis times, and other solver options.
The modified XML is then saved to the specified setup file.

## Usage

``` r
setup_body_kinematics_xml(
  base_file = "setup_bk_base.xml",
  model_file = "model_scaled.osim",
  replace_force_set = "false",
  force_set_files = "reserve_actuators.xml",
  results_directory = ".",
  output_precision = "12",
  start_time,
  end_time,
  solve_for_equilibrium_for_auxiliary_states = "false",
  maximum_number_of_integrator_steps = "20000",
  maximum_integrator_step_size = "1",
  minimum_integrator_step_size = "1e-05",
  integrator_error_tolerance = "1e-05",
  step_interval = "1",
  in_degrees = "true",
  bodies = "all",
  express_results_in_body_local_frame = "false",
  external_loads_file = "external_loads.xml",
  coordinates_file = "ik.mot",
  lowpass_cutoff_frequency_for_coordinates = "8",
  setup_filename = "setup_bk.xml"
)
```

## Arguments

- base_file:

  Character string. The path to the base XML file to be modified
  (default is "setup_bk_base.xml").

- model_file:

  Character string. The path to the model file to be used (default is
  "model_scaled.osim").

- replace_force_set:

  Character string. Whether to replace the force set ("true" or "false",
  default is "false").

- force_set_files:

  Character string. The path to the force set files to be used (default
  is "reserve_actuators.xml").

- results_directory:

  Character string. The directory where results should be saved (default
  is current directory, ".").

- output_precision:

  Character string. The desired precision for output (default is "12").

- start_time:

  Numeric. The start time of the analysis (in seconds).

- end_time:

  Numeric. The end time of the analysis (in seconds).

- solve_for_equilibrium_for_auxiliary_states:

  Character string. Whether to solve for equilibrium for auxiliary
  states ("true" or "false", default is "false").

- maximum_number_of_integrator_steps:

  Numeric. The maximum number of integrator steps (default is 20000).

- maximum_integrator_step_size:

  Numeric. The maximum size of each integrator step (default is 1).

- minimum_integrator_step_size:

  Numeric. The minimum size of each integrator step (default is 1e-05).

- integrator_error_tolerance:

  Numeric. The error tolerance for the integrator (default is 1e-05).

- step_interval:

  Numeric. The step interval for the body kinematics analysis (default
  is "1").

- in_degrees:

  Character string. Whether the results should be expressed in degrees
  ("true" or "false", default is "true").

- bodies:

  Character string. Which bodies to include in the analysis (default is
  "all").

- express_results_in_body_local_frame:

  Character string. Whether to express results in the body local frame
  ("true" or "false", default is "false").

- external_loads_file:

  Character string. The path to the external loads file (default is
  "external_loads.xml").

- coordinates_file:

  Character string. The path to the coordinates file (default is
  "ik.mot").

- lowpass_cutoff_frequency_for_coordinates:

  Numeric. The cutoff frequency for lowpass filtering of coordinates
  (default is "8").

- setup_filename:

  Character string. The name of the XML file to save the modified
  configuration (default is "setup_bk.xml").

## Value

This function does not return any value. It modifies the XML file in
place and saves it to the provided \`setup_filename\`.

## Details

This function uses the \`XML\` package to parse and modify the XML
configuration file. Each parameter in the function corresponds to a
specific element in the XML, and its value is updated accordingly. The
modified XML file is then saved to the specified location.
