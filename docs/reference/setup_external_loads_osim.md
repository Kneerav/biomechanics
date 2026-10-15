# Create an OpenSim External Loads XML File

This function creates an OpenSim External Loads XML setup file for
applying external forces (e.g., ground reaction forces) to a model. It
is useful for preparing input files for inverse dynamics or forward
simulations.

## Usage

``` r
setup_external_loads_osim(
  setup_filename = "external_loads.xml",
  set_name = c("right", "left"),
  set_applied_body_name = c("calcn_r", "calcn_l"),
  force_expressed_in_body_name = "ground",
  point_expressed_in_body_name = "ground",
  set_force_identifier = c("X1_force_", "X2_force_"),
  set_point_identifier = c("X1_point_", "X2_point_"),
  set_torque_identifier = c("X1_torque_", "X2_torque_"),
  data_source_name = "grf.mot",
  write_file = TRUE,
  return_object = FALSE
)
```

## Arguments

- setup_filename:

  Character. Name of the XML file to write (default:
  \`"external_loads.xml"\`).

- set_name:

  Character vector. Names of the external load elements (e.g.,
  \`"right"\`, \`"left"\`).

- set_applied_body_name:

  Character vector. Names of the bodies to which each load is applied
  (e.g., \`"calcn_r"\`).

- force_expressed_in_body_name:

  Character. Body in which forces are expressed.

- point_expressed_in_body_name:

  Character. Body in which point locations are expressed.

- set_force_identifier:

  Character vector. Identifiers for force data columns in the \`.mot\`
  file (e.g., \`"fx\_"\`).

- set_point_identifier:

  Character vector. Identifiers for point location columns in the
  \`.mot\` file (e.g., \`"px\_"\`).

- set_torque_identifier:

  Character vector. Identifiers for torque data columns in the \`.mot\`
  file (e.g., \`"tx\_"\`).

- data_source_name:

  Character. Name of the \`.mot\` file providing the external load data.

- write_file:

  Logical. If \`TRUE\`, writes the XML file to disk (default: \`TRUE\`).

- return_object:

  Logical. If \`TRUE\`, returns the \`ExternalLoads\` OpenSim object
  (default: \`FALSE\`).

## Value

Invisibly returns the \`opensim::ExternalLoads\` object if
\`return_object = TRUE\`.

## Details

Each external load set in OpenSim must specify: \* a \*\*body\*\* the
load is applied to, \* \*\*force/point/torque identifiers\*\* (prefixes
in the \`.mot\` file), \* and the \*\*data source\*\* (e.g., GRF
\`.mot\` file).

This function automates building such XML configurations, supporting
multiple load sets (e.g., left and right foot).
