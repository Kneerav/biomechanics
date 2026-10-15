# Setup Kinematic Analysis for OpenSim

This function configures and sets up a kinematic analysis in OpenSim for
a given model. The analysis runs using the specified parameters, with
options to save results and return the analysis tool object for further
use.

## Usage

``` r
setup_kinematic_analysis_osim(
  model_file = "model_scaled.osim",
  replace_force_set = FALSE,
  force_set_file = "reserve_actuators.xml",
  results_directory = ".",
  start_time = 0,
  end_time = 3,
  solve_for_equilibrium_for_auxiliary_states = TRUE,
  coordinates_file = NULL,
  controls_file = NULL,
  states_file = NULL,
  in_degrees = TRUE,
  step_interval = 1,
  setup_filename = "setup_ka.xml",
  write_file = FALSE,
  return_object = TRUE
)
```

## Arguments

- model_file:

  A string specifying the path to the OpenSim model file. Default is
  "model_scaled.osim".

- replace_force_set:

  A logical indicating whether to replace the force set. Default is
  FALSE.

- force_set_file:

  A string specifying the path to the force set file (e.g.,
  "reserve_actuators.xml").

- results_directory:

  A string specifying the directory where results will be saved. Default
  is the current directory ("./").

- start_time:

  A numeric value indicating the start time for the analysis. Default is
  0.

- end_time:

  A numeric value indicating the end time for the analysis. Default is
  3.

- solve_for_equilibrium_for_auxiliary_states:

  A logical indicating whether to solve for equilibrium for auxiliary
  states. Default is TRUE.

- coordinates_file:

  A string specifying the path to the coordinates file. Default is NULL.

- controls_file:

  A string specifying the path to the controls file. Default is NULL.

- states_file:

  A string specifying the path to the states file. Default is NULL.

- in_degrees:

  A logical indicating whether to use degrees for coordinate values.
  Default is TRUE.

- step_interval:

  A numeric value specifying the step interval for the analysis. Default
  is 1.

- setup_filename:

  A string specifying the filename for the XML setup file. Default is
  "setup_ka.xml".

- write_file:

  A logical indicating whether to write the analysis tool to an XML
  file. Default is FALSE.

- return_object:

  A logical indicating whether to return the analysis tool object.
  Default is TRUE.

## Value

If \`return_object\` is TRUE, returns the OpenSim \`AnalyzeTool\`
object. If neither \`write_file\` nor \`return_object\` is TRUE, returns
NULL invisibly.
