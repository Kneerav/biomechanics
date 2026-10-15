# Extract muscle force directions from an OpenSim model

Calculates anatomical and/or effective muscle attachment locations and
normalised muscle force-direction vectors for selected physical frames
in an OpenSim musculoskeletal model.

## Usage

``` r
muscle_force_direction(
  model_file = "model.osim",
  kinematics_file = NULL,
  target_frame_names = "femur_r",
  output_frame_name = "femur_r",
  line_of_action_mode = "effective",
  selected_muscle_names = NULL,
  file_output = "muscle_force_directions.csv",
  kinematics_in_degrees = TRUE,
  strict_kinematics_columns = FALSE,
  equilibrate_muscles = FALSE,
  print_progress = TRUE,
  write_file = T,
  return_object = F
)
```

## Arguments

- model_file:

  A character string giving the path to an OpenSim model file, normally
  with extension `.osim`. The file must exist and be readable by the
  OpenSim Python package.

- kinematics_file:

  Either a character string giving the path to an OpenSim motion or
  states file, normally with extension `.mot` or `.sto`, or \`NULL\`.
  When a file is supplied, the function evaluates the muscle paths at
  every kinematic frame in that file. When \`NULL\`, the model is
  evaluated once using the default coordinate values established by
  \`Model\$initSystem()\`.

- target_frame_names:

  A character vector containing the names of the bodies or physical
  frames on which the muscle forces act. Body names such as
  \`"femur_r"\` may be supplied directly. Use \`"ground"\` for the
  OpenSim ground frame. An absolute OpenSim component path may be used
  for a physical frame that is not directly accessible by body name.

  Each returned force vector points away from its attachment on the
  target frame and toward the remainder of the muscle path.

- output_frame_name:

  A single character string specifying the reference frame in which
  attachment coordinates, force-direction vectors, and transport moments
  are expressed. Use \`"ground"\` to return global coordinates, a body
  name such as \`"femur_r"\` to return body-local coordinates, or an
  absolute OpenSim component path for another physical frame.

- line_of_action_mode:

  A single character string specifying which attachment definition to
  return. Must be one of:

  \`"anatomical"\`

  :   Return the terminal anatomical attachment and the direction from
      that attachment toward the immediately adjacent point in the
      evaluated muscle path.

  \`"effective"\`

  :   Return the effective attachment at the boundary of the group of
      current path points attached to the target frame, together with
      the direction toward the first path point outside that frame.

  \`"both"\`

  :   Return separate anatomical and effective rows for each eligible
      muscle, target frame, and kinematic frame.

- selected_muscle_names:

  Either \`NULL\` or a character vector containing exact OpenSim muscle
  names, for example \`c("addlong_r", "semiten_r")\`. When \`NULL\`, all
  muscles in the model are inspected. Muscle names must match the names
  stored in \`model\$getMuscles()\`.

- file_output:

  A character string giving the path of the CSV file to create when
  \`write_file = TRUE\`. Parent directories are not created
  automatically unless this is explicitly implemented in the function.
  The argument is ignored when \`write_file = FALSE\`.

- kinematics_in_degrees:

  A logical value or \`NA\`.

  \* \`TRUE\` indicates that rotational-coordinate values in
  \`kinematics_file\` are expressed in degrees and must be converted to
  radians before being assigned to the OpenSim state. \* \`FALSE\`
  indicates that rotational-coordinate values are already in radians. \*
  \`NA\` instructs the function to inspect the \`inDegrees\` metadata
  field in the OpenSim table. If that metadata is absent, the
  implementation should fall back to radians.

  Translational coordinates are not converted.

- strict_kinematics_columns:

  Logical. If \`FALSE\`, columns in the kinematics file that cannot be
  matched to model coordinates are ignored. If \`TRUE\`, the function
  stops when it encounters an unmatched, non-speed column. Columns
  ending in \`"/speed"\` are ignored because the muscle-path calculation
  requires only Position-stage model realisation.

- equilibrate_muscles:

  Logical. If \`TRUE\`, \`model\$equilibrateMuscles(state)\` is called
  after applying each kinematic frame. Muscle equilibration is generally
  unnecessary when only attachment positions and force directions are
  required, and may fail for physiologically or numerically problematic
  states. The default is \`FALSE\`.

- print_progress:

  Logical. If \`TRUE\`, print progress messages while processing
  kinematic frames and report muscles skipped because their topology is
  not handled by the MATLAB-equivalent selection logic.

- write_file:

  Logical. If \`TRUE\`, write the resulting data frame to the CSV path
  specified by \`file_output\`. If \`FALSE\`, no output file is created.

- return_object:

  Logical. If \`TRUE\`, return the result as an R data frame. If
  \`FALSE\`, return the result invisibly. Setting both \`write_file =
  FALSE\` and \`return_object = FALSE\` performs the analysis without
  providing a readily accessible result and is therefore usually not
  useful.

## Value

When \`return_object = TRUE\`, a data frame containing one row for each
eligible combination of kinematic frame, muscle, target frame, and
requested line-of-action mode. When \`return_object = FALSE\`, the same
data frame is returned invisibly.

Depending on the implemented options, columns include:

- \`time\`:

  Time corresponding to the evaluated model state.

- \`muscle\`:

  OpenSim muscle name.

- \`target_frame\`:

  Component path of the frame on which the force acts.

- \`output_frame\`:

  Component path of the frame used to express results.

- \`mode\`:

  Either \`"anatomical"\` or \`"effective"\`.

- \`side\`:

  Whether the vector points toward the start or end of the ordered
  muscle path.

- \`attachment_point\`:

  Name of the selected attachment path point.

- \`adjacent_point\`:

  Name of the path point used to define direction.

- \`point_x\`, \`point_y\`, \`point_z\`:

  Attachment coordinates expressed in \`output_frame_name\`, normally in
  metres.

- \`direction_x\`, \`direction_y\`, \`direction_z\`:

  Components of the normalised force-direction vector expressed in
  \`output_frame_name\`.

- \`transport_moment_x\`, \`transport_moment_y\`,
  \`transport_moment_z\`:

  Transport-moment components associated with relocating the effective
  unit force from the effective attachment to the anatomical attachment.
  These have dimensions of length until multiplied by a force magnitude.
  Anatomical rows contain zero.

## Details

The function accesses the OpenSim Python API through reticulate. For
each model state, it evaluates the current muscle geometry path and
reproduces the attachment-selection logic used by the the
MuscleForceDirection tool
(https://github.com/modenaxe/MuscleForceDirection/tree/master).

Anatomical force directions are calculated at the terminal anatomical
attachment of the muscle on the target frame. Effective force directions
are calculated at the boundary point where the current muscle path
enters or leaves the target frame.

All returned force-direction vectors point from the selected target
frame outward into the remainder of the muscle path. Direction vectors
are normalised to unit length.

The function requires a Python environment containing a compatible
installation of the \`opensim\` Python module. It does not install
OpenSim or select a Python or Conda environment automatically.

Users should configure reticulate before calling the function, for
example with \`reticulate::use_condaenv()\` or
\`reticulate::use_python()\`. Python selection normally must occur
before Python is initialised in the current R session. reticulate
supports selecting Python, virtual environments, and Conda environments.

Both anatomical and effective results are derived from the same
evaluated current muscle path. The implementation assumes that muscle
path points are ordered from proximal to distal, matching the MATLAB
MuscleForceDirection implementation.

A muscle is skipped for a target frame when neither its first nor final
current path point is attached to that frame, even if intermediate via
points are attached to it. A muscle that both starts and ends on the
same target frame is also skipped because this topology is not defined
by the MATLAB implementation.

Attachment coordinates are points, whereas force directions are free
vectors. To avoid applying translation to a direction vector, the
function transforms the attachment and adjacent point into the requested
output frame, subtracts the transformed points, and then normalises the
resulting vector.

The returned direction components do not represent a second point. To
create an arrow endpoint of length \`L\`, calculate:

“\` end_x \<- point_x + L \* direction_x end_y \<- point_y + L \*
direction_y end_z \<- point_z + L \* direction_z “\`

## Python requirements

The selected Python environment must contain the \`opensim\` module. The
function imports that module with \`reticulate::import()\`. The
reticulate import interface makes a Python module available as an R
object whose attributes and methods can be accessed with \`\$\`.

## Coordinate units

OpenSim models conventionally represent distances in metres and
rotational coordinates internally in radians. The function converts
rotational kinematics from degrees when \`kinematics_in_degrees =
TRUE\`.

Direction vectors are dimensionless. The transport-moment output
calculated from a unit force direction and an attachment displacement
has dimensions of length. Multiplying it by a muscle force in newtons
produces a moment in newton-metres.

\#' @references van Arkel, R. J., Modenese, L., Phillips, A. T., &
Jeffers, J. R. (2013). Hip abduction can prevent posterior edge loading
of hip replacements. Journal of orthopaedic research : official
publication of the Orthopaedic Research Society, 31(8), 1172–1179.
https://doi.org/10.1002/jor.22364 Phillips, A. T. M., Villette, C. C., &
Modenese, L. (2015). Femoral bone mesoscale structural architecture
prediction using musculoskeletal and finite element modelling.
International Biomechanics, 2(1), 43–61.
https://doi.org/10.1080/23335432.2015.1017609
