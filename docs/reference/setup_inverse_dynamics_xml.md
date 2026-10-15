# Set Up Inverse Dynamics XML for Simulation

This function modifies an XML configuration file for inverse dynamics
analysis. It updates the XML file with various parameters such as model
file, external loads, analysis times, forces to exclude, and other
configuration settings specific to inverse dynamics. The modified XML
file is then saved to the specified setup filename.

## Usage

``` r
setup_inverse_dynamics_xml(
  base_file = "setup_id_base.xml",
  results_directory = ".",
  model_file = "model_scaled.osim",
  start_time,
  end_time,
  forces_to_exclude = "Muscles",
  external_loads_file = "external_loads.xml",
  coordinates_file = "ik.mot",
  lowpass_cutoff_frequency_for_coordinates = "8",
  output_gen_force_file = "inverse_dynamics.sto",
  setup_filename = "setup_id.xml"
)
```

## Arguments

- base_file:

  Character string. The path to the base XML file to be modified
  (default is "setup_id_base.xml").

- results_directory:

  Character string. The directory where results should be saved (default
  is current directory, ".").

- model_file:

  Character string. The path to the model file to be used (default is
  "model_scaled.osim").

- start_time:

  Numeric. The start time of the analysis (in seconds).

- end_time:

  Numeric. The end time of the analysis (in seconds).

- forces_to_exclude:

  Character string. Forces to exclude during the analysis (default is
  "Muscles").

- external_loads_file:

  Character string. The path to the external loads file (default is
  "external_loads.xml").

- coordinates_file:

  Character string. The path to the coordinates file (default is
  "ik.mot").

- lowpass_cutoff_frequency_for_coordinates:

  Numeric. The cutoff frequency for lowpass filtering of coordinates
  (default is "8").

- output_gen_force_file:

  Character string. The name of the output file for generalized forces
  (default is "inverse_dynamics.sto").

- setup_filename:

  Character string. The name of the XML file to save the modified
  configuration (default is "setup_id.xml").

## Value

This function does not return any value. It modifies the XML file in
place and saves it to the provided \`setup_filename\`.

## Details

This function uses the \`XML\` package to parse and modify the XML
configuration file. Each parameter in the function corresponds to a
specific element in the XML, and its value is updated accordingly. The
modified XML file is then saved to the specified location.
