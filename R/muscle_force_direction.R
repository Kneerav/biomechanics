#' Extract muscle force directions from an OpenSim model
#'
#' Calculates anatomical and/or effective muscle attachment locations and
#' normalised muscle force-direction vectors for selected physical frames in
#' an OpenSim musculoskeletal model.
#'
#' The function accesses the OpenSim Python API through
#' \pkg{reticulate}. For each model state, it evaluates the current muscle
#' geometry path and reproduces the attachment-selection logic used by the
#'the MuscleForceDirection tool (https://github.com/modenaxe/MuscleForceDirection/tree/master).
#'
#' Anatomical force directions are calculated at the terminal anatomical
#' attachment of the muscle on the target frame. Effective force directions
#' are calculated at the boundary point where the current muscle path enters
#' or leaves the target frame.
#'
#' All returned force-direction vectors point from the selected target frame
#' outward into the remainder of the muscle path. Direction vectors are
#' normalised to unit length.
#'
#' @param model_file A character string giving the path to an OpenSim
#'   model file, normally with extension \file{.osim}. The file must exist and
#'   be readable by the OpenSim Python package.
#'
#' @param kinematics_file Either a character string giving the path to an
#'   OpenSim motion or states file, normally with extension \file{.mot} or
#'   \file{.sto}, or `NULL`. When a file is supplied, the function evaluates
#'   the muscle paths at every kinematic frame in that file. When `NULL`, the
#'   model is evaluated once using the default coordinate values established
#'   by `Model$initSystem()`.
#'
#' @param target_frame_names A character vector containing the names of the
#'   bodies or physical frames on which the muscle forces act. Body names such
#'   as `"femur_r"` may be supplied directly. Use `"ground"` for the OpenSim
#'   ground frame. An absolute OpenSim component path may be used for a
#'   physical frame that is not directly accessible by body name.
#'
#'   Each returned force vector points away from its attachment on the target
#'   frame and toward the remainder of the muscle path.
#'
#' @param output_frame_name A single character string specifying the reference
#'   frame in which attachment coordinates, force-direction vectors, and
#'   transport moments are expressed. Use `"ground"` to return global
#'   coordinates, a body name such as `"femur_r"` to return body-local
#'   coordinates, or an absolute OpenSim component path for another physical
#'   frame.
#'
#' @param line_of_action_mode A single character string specifying which
#'   attachment definition to return. Must be one of:
#'
#'   \describe{
#'     \item{`"anatomical"`}{
#'       Return the terminal anatomical attachment and the direction from that
#'       attachment toward the immediately adjacent point in the evaluated
#'       muscle path.
#'     }
#'     \item{`"effective"`}{
#'       Return the effective attachment at the boundary of the group of
#'       current path points attached to the target frame, together with the
#'       direction toward the first path point outside that frame.
#'     }
#'     \item{`"both"`}{
#'       Return separate anatomical and effective rows for each eligible
#'       muscle, target frame, and kinematic frame.
#'     }
#'   }
#'
#' @param selected_muscle_names Either `NULL` or a character vector containing
#'   exact OpenSim muscle names, for example
#'   `c("addlong_r", "semiten_r")`. When `NULL`, all muscles in the model are
#'   inspected. Muscle names must match the names stored in
#'   `model$getMuscles()`.
#'
#' @param file_output A character string giving the path of the CSV file to
#'   create when `write_file = TRUE`. Parent directories are not created
#'   automatically unless this is explicitly implemented in the function.
#'   The argument is ignored when `write_file = FALSE`.
#'
#' @param kinematics_in_degrees A logical value or `NA`.
#'
#'   * `TRUE` indicates that rotational-coordinate values in
#'     `kinematics_file` are expressed in degrees and must be converted to
#'     radians before being assigned to the OpenSim state.
#'   * `FALSE` indicates that rotational-coordinate values are already in
#'     radians.
#'   * `NA` instructs the function to inspect the `inDegrees` metadata field
#'     in the OpenSim table. If that metadata is absent, the implementation
#'     should fall back to radians.
#'
#'   Translational coordinates are not converted.
#'
#' @param strict_kinematics_columns Logical. If `FALSE`, columns in the
#'   kinematics file that cannot be matched to model coordinates are ignored.
#'   If `TRUE`, the function stops when it encounters an unmatched,
#'   non-speed column. Columns ending in `"/speed"` are ignored because the
#'   muscle-path calculation requires only Position-stage model realisation.
#'
#' @param equilibrate_muscles Logical. If `TRUE`,
#'   `model$equilibrateMuscles(state)` is called after applying each kinematic
#'   frame. Muscle equilibration is generally unnecessary when only attachment
#'   positions and force directions are required, and may fail for
#'   physiologically or numerically problematic states. The default is
#'   `FALSE`.
#'
#' @param print_progress Logical. If `TRUE`, print progress messages while
#'   processing kinematic frames and report muscles skipped because their
#'   topology is not handled by the MATLAB-equivalent selection logic.
#'
#' @param write_file Logical. If `TRUE`, write the resulting data frame to the
#'   CSV path specified by `file_output`. If `FALSE`, no output file is
#'   created.
#'
#' @param return_object Logical. If `TRUE`, return the result as an R data
#'   frame. If `FALSE`, return the result invisibly. Setting both
#'   `write_file = FALSE` and `return_object = FALSE` performs the analysis
#'   without providing a readily accessible result and is therefore usually
#'   not useful.
#'
#' @return When `return_object = TRUE`, a data frame containing one row for
#'   each eligible combination of kinematic frame, muscle, target frame, and
#'   requested line-of-action mode. When `return_object = FALSE`, the same
#'   data frame is returned invisibly.
#'
#'   Depending on the implemented options, columns include:
#'
#'   \describe{
#'     \item{`time`}{Time corresponding to the evaluated model state.}
#'     \item{`muscle`}{OpenSim muscle name.}
#'     \item{`target_frame`}{Component path of the frame on which the force acts.}
#'     \item{`output_frame`}{Component path of the frame used to express results.}
#'     \item{`mode`}{Either `"anatomical"` or `"effective"`.}
#'     \item{`side`}{Whether the vector points toward the start or end of the
#'       ordered muscle path.}
#'     \item{`attachment_point`}{Name of the selected attachment path point.}
#'     \item{`adjacent_point`}{Name of the path point used to define direction.}
#'     \item{`point_x`, `point_y`, `point_z`}{Attachment coordinates expressed
#'       in `output_frame_name`, normally in metres.}
#'     \item{`direction_x`, `direction_y`, `direction_z`}{Components of the
#'       normalised force-direction vector expressed in `output_frame_name`.}
#'     \item{`transport_moment_x`, `transport_moment_y`,
#'       `transport_moment_z`}{Transport-moment components associated with
#'       relocating the effective unit force from the effective attachment to
#'       the anatomical attachment. These have dimensions of length until
#'       multiplied by a force magnitude. Anatomical rows contain zero.}
#'   }
#'
#' @details
#' The function requires a Python environment containing a compatible
#' installation of the `opensim` Python module. It does not install OpenSim or
#' select a Python or Conda environment automatically.
#'
#' Users should configure \pkg{reticulate} before calling the function, for
#' example with `reticulate::use_condaenv()` or
#' `reticulate::use_python()`. Python selection normally must occur before
#' Python is initialised in the current R session. \pkg{reticulate} supports
#' selecting Python, virtual environments, and Conda environments.
#'
#' Both anatomical and effective results are derived from the same evaluated
#' current muscle path. The implementation assumes that muscle path points are
#' ordered from proximal to distal, matching the MATLAB
#' MuscleForceDirection implementation.
#'
#' A muscle is skipped for a target frame when neither its first nor final
#' current path point is attached to that frame, even if intermediate via
#' points are attached to it. A muscle that both starts and ends on the same
#' target frame is also skipped because this topology is not defined by the
#' MATLAB implementation.
#'
#' Attachment coordinates are points, whereas force directions are free
#' vectors. To avoid applying translation to a direction vector, the function
#' transforms the attachment and adjacent point into the requested output
#' frame, subtracts the transformed points, and then normalises the resulting
#' vector.
#'
#' The returned direction components do not represent a second point. To
#' create an arrow endpoint of length `L`, calculate:
#'
#' ```
#' end_x <- point_x + L * direction_x
#' end_y <- point_y + L * direction_y
#' end_z <- point_z + L * direction_z
#' ```
#'
#' @section Python requirements:
#' The selected Python environment must contain the `opensim` module. The
#' function imports that module with `reticulate::import()`. The
#' \pkg{reticulate} import interface makes a Python module available as an R
#' object whose attributes and methods can be accessed with `$`.
#'
#' @section Coordinate units:
#' OpenSim models conventionally represent distances in metres and rotational
#' coordinates internally in radians. The function converts rotational
#' kinematics from degrees when `kinematics_in_degrees = TRUE`.
#'
#' Direction vectors are dimensionless. The transport-moment output calculated
#' from a unit force direction and an attachment displacement has dimensions
#' of length. Multiplying it by a muscle force in newtons produces a moment in
#' newton-metres.
#'
#' #' @references
#' van Arkel, R. J., Modenese, L., Phillips, A. T., & Jeffers, J. R. (2013). Hip abduction can prevent posterior edge loading of hip replacements. Journal of orthopaedic research : official publication of the Orthopaedic Research Society, 31(8), 1172–1179. https://doi.org/10.1002/jor.22364
#' Phillips, A. T. M., Villette, C. C., & Modenese, L. (2015). Femoral bone mesoscale structural architecture prediction using musculoskeletal and finite element modelling. International Biomechanics, 2(1), 43–61. https://doi.org/10.1080/23335432.2015.1017609
#'
#' @importFrom reticulate import py_has_attr py_is_null_xptr py_module_available
#' @export
muscle_force_direction <- function(
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
){

  # Muscle force direction extraction using OpenSim through reticulate
  #
  # It extracts muscle attachment locations and unit line-of-action vectors for
  # one or more bodies, at either the model default pose or every row of a .mot/
  # .sto kinematics file. Main inspiration was Luca Modenese MATLAB code found
  # here: https://github.com/modenaxe/MuscleForceDirection/tree/master
  #
  # Direction convention:
  #   The vector points from the attachment on the selected body into the muscle
  #   path. It therefore represents the tensile force direction acting on that
  #   body at the attachment.
  #
  # Limitations:
  #   * The anatomical and effective attachment definitions follow the
  #     MuscleForceDirection implementation from Modenese.
  #   * Muscles having only intermediate via points on the target body, with
  #     neither endpoint attached to that body, are skipped.


  # Check if 'opensim' and 'scipy' modules are available in the Python environment
  if (!reticulate::py_module_available("opensim")) {
    stop("Error: The 'opensim' Python package is not installed. Please install it and try again.")
  }

  osim <- reticulate::import("opensim")

  ### check inputs ###
  if (!file.exists(model_file)) {
    stop("Model file does not exist: ", model_file)
  }
  if (!is.null(kinematics_file) && !file.exists(kinematics_file)) {
    stop("Kinematics file does not exist: ", kinematics_file)
  }

  line_of_action_mode <- tolower(line_of_action_mode)
  if (!line_of_action_mode %in% c("anatomical", "effective", "both")) {
    stop("line_of_action_mode must be 'anatomical', 'effective', or 'both'.")
  }

  #################################### get osim basics ###############################
  model <- osim$Model(model_file)
  state <- model$initSystem()
  ground <- model$getGround()
  body_set <- model$getBodySet()
  muscle_set <- model$getMuscles()
  coordinate_set <- model$getCoordinateSet()

  #################################### convenience functions  ####################################
  collection_size <- function(x) {
    if (reticulate::py_has_attr(x, "getSize")) return(as.integer(x$getSize()))
    if (reticulate::py_has_attr(x, "size")) return(as.integer(x$size()))
    stop("OpenSim collection exposes neither getSize() nor size().")
  }

  collection_get <- function(x, i_zero_based) {
    x$get(as.integer(i_zero_based))
  }

  vec3_to_numeric <- function(v) {
    c(
      as.numeric(v$get(0L)),
      as.numeric(v$get(1L)),
      as.numeric(v$get(2L))
    )
  }

  component_name <- function(x) {
    as.character(x$getName())
  }

  component_path <- function(x) {
    as.character(x$getAbsolutePathString())
  }

  same_frame <- function(a, b) {
    identical(component_path(a), component_path(b))
  }

  #################################### resolve frames  ####################################
  resolve_frame <- function(frame_name) {

    #catch ground
    if (frame_name %in% c("ground", "/ground")) return(ground)

    #catch / related names
    if (startsWith(frame_name, "/")) {
      component <- model$getComponent(frame_name)
      frame <- osim$PhysicalFrame$safeDownCast(component)
      if (is.null(frame) || reticulate::py_is_null_xptr(frame)) {
        stop("Component is not a PhysicalFrame: ", frame_name)
      }
      return(frame)
    }

    #most common case: a Body name such as femur_r.
    frame <- tryCatch(body_set$get(frame_name), error = function(e) NULL)
    if (!is.null(frame)) return(frame)

    stop(
      "PhysicalFrame '", frame_name, "' was not found as ground, an absolute ",
      "component path, or a Body name. For offset frames, use the absolute ",
      "component path."
    )
  }

  output_frame <- resolve_frame(output_frame_name)
  target_frames <- lapply(target_frame_names, resolve_frame)

  ############################## get muscles as osim objects  ###############################
  if (is.null(selected_muscle_names)) {
    n_muscles <- collection_size(muscle_set)
    muscle_indices <- if (n_muscles > 0L) 0:(n_muscles - 1L) else integer(0)
    muscles <- lapply(muscle_indices, function(i) collection_get(muscle_set, i))
  } else {
    muscles <- lapply(selected_muscle_names, function(x) muscle_set$get(x))
  }

  #################################### line of action  ####################################
  modes_to_run <- if (line_of_action_mode == "both") {
    c("anatomical", "effective")
  } else {
    line_of_action_mode
  }

  #################################### read kinematics ####################################

  #if null, use models default pose
  if (is.null(kinematics_file)) {

    kinematics_table <- NULL
    times <- as.numeric(state$getTime())
    use_degrees <- FALSE

  } else {

    #if file specified, use file
    kinematics_table <- osim$TimeSeriesTable(kinematics_file)
    independent_column <- kinematics_table$getIndependentColumn()
    n_times <- kinematics_table$getNumRows()
    time_indices <- if (n_times > 0L) seq(0L, (n_times - 1L), 1L) else integer(0)
    times <- unlist(independent_column)

    #determine if in degrees if not specified (NA)
    if (is.na(kinematics_in_degrees)) {

      metadata_value <- tryCatch(
        as.character(kinematics_table$getTableMetaDataString("inDegrees")),
        error = function(e) "false"

      )
      use_degrees <- tolower(trimws(metadata_value)) %in% c("yes", "true", "1")
    } else {
      use_degrees <- isTRUE(kinematics_in_degrees)
    }
  }

  #stop if no data was found in file
  if (length(times) == 0L) stop("No time rows were found to process.")

  ################################ kinematic processing ################################

  #storage
  result_rows <- list()
  result_index <- 0L

  #loop through each row of data
  #row_r is index (e.g., row_r = 1)
  for (row_r in seq_along(times)) {

    row_py <- as.integer(row_r - 1L)
    current_time <- times[[row_r]]
    state$setTime(current_time)

    #apply this row of the .mot/.sto file to the model coordinates.
    if(!is.null(kinematics_table)){

      labels_kinematics <- kinematics_table$getColumnLabels() %>% unlist()
      n_labels <- length(labels_kinematics)
      matched_coordinate_count <- 0L

      for(label_i in 1:n_labels){

        #get current label (r style)
        label <- as.character(labels_kinematics[label_i])

        #speeds are unnecessary because only position-stage geometry is needed.
        if(endsWith(label, "/speed")) next

        #fix coord name if labels have "/value"
        coordinate_name <- label
        if(endsWith(label, "/value")){
          label_parts <- strsplit(label, "/", fixed = TRUE)[[1]]
          coordinate_name <- label_parts[[length(label_parts) - 1L]]
        }

        #get coord of interest
        coordinate <- tryCatch(
          coordinate_set$get(coordinate_name),
          error = function(e) NULL
        )

        #stop if not found
        if(is.null(coordinate)){
          if(strict_kinematics_columns){
            stop(
              "Kinematics column '", label,
              "' does not match a model coordinate."
            )
          }
          next
        }

        #get coordinates column (and row value)
        column <- kinematics_table$getDependentColumn(label)$to_numpy()
        value <- column[row_r]

        #check motion type, convert to radians if needed
        motion_type <- as.integer(coordinate$getMotionType())
        rotational_type <- as.integer(osim$Coordinate$Rotational)
        if(use_degrees && motion_type == rotational_type){
          value <- value * pi / 180
        }

        #set the value
        #enforceConstraints = FALSE; realisePosition() is called after all values.
        coordinate$setValue(state, value, FALSE)
        matched_coordinate_count <- matched_coordinate_count + 1L
      }

      if(strict_kinematics_columns && matched_coordinate_count == 0L){
        stop("No kinematics columns matched coordinates in the model.")
      }
    }

    #realise to position (velocity not needed)
    model$realizePosition(state)

    #equilibriate if requested (not needed though)
    if(equilibrate_muscles){
      model$equilibrateMuscles(state)
      model$realizePosition(state)
    }

    #loop through muscles (where muscle is osim object) (i.e., muscle = muscles[[1]])
    for(muscle in muscles){

      #get name and path
      muscle_name <- component_name(muscle)
      geometry_path <- muscle$getGeometryPath()

      #Both anatomical and effective results are calculated from the state-dependent path
      path_collection <- tryCatch(
        geometry_path$getCurrentPath(state),
        error = function(e) {
          warning(
            "Could not obtain current path for ", muscle_name,
            " at time ", current_time, ": ", conditionMessage(e)
          )
          NULL
        }
      )

      #skip if no path
      if(is.null(path_collection)) next

      #get path point number
      n_path_points <- collection_size(path_collection)
      if(n_path_points < 2L) next #skip if less than two

      #evaluate each currently available path point in ground.
      path_points <- vector("list", n_path_points)
      valid_point_count <- 0L

      #point_r is index (i.e., point_r = 1)
      for(point_r in seq_len(n_path_points)) {

        #establish index for python and get the point object
        point_py <- as.integer(point_r - 1L)
        point <- collection_get(path_collection, point_py)

        point_data <- tryCatch(
          {
            #get the parent frame of the point
            parent_frame <- if(reticulate::py_has_attr(point, "getParentFrame")) {
              point$getParentFrame()
            } else {
              point$getBody()
            }

            #get point in ground frame
            point_ground <- if(reticulate::py_has_attr(point, "getLocationInGround")) {
              vec3_to_numeric(point$getLocationInGround(state))
            } else {
              local_location <- point$getLocation(state)

              vec3_to_numeric(
                parent_frame$findStationLocationInGround(
                  state,
                  local_location
                )
              )
            }

            list(
              point = point,
              name = component_name(point),
              frame = parent_frame,
              frame_path = component_path(parent_frame),
              ground = point_ground
            )
          },
          error = function(e) NULL
        )

        #if not NULL, add to valid point counter, and add to path_points
        if(!is.null(point_data)) {
          valid_point_count <- valid_point_count + 1L
          path_points[[valid_point_count]] <- point_data
        }
      }

      #skip if less than 2 valid points
      if(valid_point_count < 2L) next

      #get valid path points only
      path_points <- path_points[seq_len(valid_point_count)]

      #loop through target frames (osim objects e.g., target_frame = target_frames[[1]])
      for(target_frame in target_frames){

        #get component path (string)
        target_path <- component_path(target_frame)

        #check if each point is on the target frame
        point_is_on_target <- vapply(
          path_points,
          function(p) same_frame(p$frame, target_frame),
          logical(1)
        )

        #check which ones are on target frame, and skip if none
        target_indices <- which(point_is_on_target)
        if(length(target_indices) == 0L) next

        #proximal and distal attachments on the target body.
        #assumes that muscle points are ordered from proximal to distal.
        prox_attach <- min(target_indices)
        dist_attach <- max(target_indices)

        n_current_points <- length(path_points)

        #CASE 1:
        #The muscle has intermediate via points on the target body, but neither
        #its first nor final point is attached to the target body. SKIP.
        if(
          !point_is_on_target[[1L]] &&
          !point_is_on_target[[n_current_points]]
        ) {
          if(print_progress) {
            message(
              muscle_name,
              " has only intermediate via points on ",
              target_path,
              ". Skipping."
            )
          }

          next
        }

        #It would be unusual for both muscle endpoints to be attached to the
        #same target body, but we SKIP if so.
        if(
          point_is_on_target[[1L]] &&
          point_is_on_target[[n_current_points]]
        ) {
          warning(
            muscle_name,
            " starts and ends on ",
            target_path,
            ". so it was skipped."
          )

          next
        }

        #CASE 2: muscle starts on the target body.
        if(
          point_is_on_target[[1L]] &&
          !point_is_on_target[[n_current_points]]
        ) {
          #anatomical attachment is the first muscle point
          anatomical_attachment_index <- 1L

          #effective attachment is the final point attached to the target body
          effective_attachment_index <- dist_attach

          #anatomical direction goes from the first to the second path point
          anatomical_neighbour_index <- 2L

          #effective direction goes from the final target-body point to the
          #first path point outside the target body
          effective_neighbour_index <- dist_attach + 1L

          anatomical_side <- "toward_path_end"
          effective_side <- "toward_path_end"
        }

        #CASE 3: muscle ends on the target body.
        if(
          !point_is_on_target[[1L]] &&
          point_is_on_target[[n_current_points]]
        ) {
          #anatomical attachment is the final muscle point
          anatomical_attachment_index <- n_current_points

          #effective attachment is the first point attached to the target body
          effective_attachment_index <- prox_attach

          #anatomical direction goes from the final to the penultimate path point
          anatomical_neighbour_index <- n_current_points - 1L

          #effective direction goes from the first target-body point to the
          #preceding path point outside the target body
          effective_neighbour_index <- prox_attach - 1L

          anatomical_side <- "toward_path_start"
          effective_side <- "toward_path_start"
        }

        #get the four path points used by the implementation
        anatomical_attachment <-
          path_points[[anatomical_attachment_index]]

        effective_attachment <-
          path_points[[effective_attachment_index]]

        anatomical_neighbour <-
          path_points[[anatomical_neighbour_index]]

        effective_neighbour <-
          path_points[[effective_neighbour_index]]

        #transform a point from ground into the selected output frame.
        #all four points are transformed before directions are calculated.
        #ensures that points belonging to different parent frames are
        #expressed in the same coordinate system before subtraction.
        transform_ground_point_to_output <- function(point_ground) {

          if(same_frame(output_frame, ground)) {
            return(point_ground)
          }

          point_vec3 <- osim$Vec3(
            point_ground[[1]],
            point_ground[[2]],
            point_ground[[3]]
          )

          vec3_to_numeric(
            ground$findStationLocationInAnotherFrame(
              state,
              point_vec3,
              output_frame
            )
          )
        }

        anatomical_attachment_output <-
          transform_ground_point_to_output(
            anatomical_attachment$ground
          )

        effective_attachment_output <-
          transform_ground_point_to_output(
            effective_attachment$ground
          )

        anatomical_neighbour_output <-
          transform_ground_point_to_output(
            anatomical_neighbour$ground
          )

        effective_neighbour_output <-
          transform_ground_point_to_output(
            effective_neighbour$ground
          )

        #compute anatomical direction of vector
        anatomical_direction <- anatomical_neighbour_output - anatomical_attachment_output
        anatomical_direction_length <- sqrt(sum(anatomical_direction^2))

        if(!is.finite(anatomical_direction_length) || anatomical_direction_length <= 1e-12){
          warning(
            "Zero-length anatomical direction skipped for ",
            muscle_name,
            " at time ",
            current_time
          )

          next
        }

        #normalise anatomical vector (pracma::Norm(anatomical_direction) should be 1 after this)
        anatomical_direction <- anatomical_direction / anatomical_direction_length

        #compute effective direction of vector
        effective_direction <- effective_neighbour_output - effective_attachment_output
        effective_direction_length <- sqrt(sum(effective_direction^2))

        if(!is.finite(effective_direction_length) || effective_direction_length <= 1e-12){
          warning(
            "Zero-length effective direction skipped for ",
            muscle_name,
            " at time ",
            current_time
          )

          next
        }

        #normalise effective vector
        effective_direction <- effective_direction / effective_direction_length

        #computing transport moment at the anatomical bone attachment
        #use same convention as luca modenese, but maybe should be reversed to cross(r, F)
        #cross(effective_direction, effective_attachment - bone_attachment)
        bone_to_effective_attachment <- effective_attachment_output - anatomical_attachment_output

        transport_moment <- c(
          effective_direction[[2]] *
            bone_to_effective_attachment[[3]] -
            effective_direction[[3]] *
            bone_to_effective_attachment[[2]],

          effective_direction[[3]] *
            bone_to_effective_attachment[[1]] -
            effective_direction[[1]] *
            bone_to_effective_attachment[[3]],

          effective_direction[[1]] *
            bone_to_effective_attachment[[2]] -
            effective_direction[[2]] *
            bone_to_effective_attachment[[1]]
        )

        #add anatomical result if requested
        if("anatomical" %in% modes_to_run) {

          result_index <- result_index + 1L

          result_rows[[result_index]] <- data.frame(
            time = current_time,
            muscle = muscle_name,
            target_frame = target_path,
            output_frame = component_path(output_frame),
            mode = "anatomical",
            side = anatomical_side,
            attachment_point = anatomical_attachment$name,
            adjacent_point = anatomical_neighbour$name,
            point_x = anatomical_attachment_output[[1]],
            point_y = anatomical_attachment_output[[2]],
            point_z = anatomical_attachment_output[[3]],
            direction_x = anatomical_direction[[1]],
            direction_y = anatomical_direction[[2]],
            direction_z = anatomical_direction[[3]],
            transport_moment_x = 0,
            transport_moment_y = 0,
            transport_moment_z = 0,
            stringsAsFactors = FALSE
          )
        }

        #add effective result if requested
        if("effective" %in% modes_to_run) {

          result_index <- result_index + 1L

          result_rows[[result_index]] <- data.frame(
            time = current_time,
            muscle = muscle_name,
            target_frame = target_path,
            output_frame = component_path(output_frame),
            mode = "effective",
            side = effective_side,
            attachment_point = effective_attachment$name,
            adjacent_point = effective_neighbour$name,
            point_x = effective_attachment_output[[1]],
            point_y = effective_attachment_output[[2]],
            point_z = effective_attachment_output[[3]],
            direction_x = effective_direction[[1]],
            direction_y = effective_direction[[2]],
            direction_z = effective_direction[[3]],
            transport_moment_x = transport_moment[[1]],
            transport_moment_y = transport_moment[[2]],
            transport_moment_z = transport_moment[[3]],
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (
      print_progress &&
      (row_r == 1L || row_r == length(times) || row_r %% 100L == 0L)
    ) {
      message(
        "Processed frame ", row_r, "/", length(times),
        " at time ", format(current_time), " s"
      )
    }
  }

  ################################### combine and save ##################################

  if (length(result_rows) == 0L) {
    results <- data.frame(
      time = numeric(0),
      muscle = character(0),
      target_frame = character(0),
      output_frame = character(0),
      mode = character(0),
      side = character(0),
      attachment_point = character(0),
      adjacent_point = character(0),
      point_x = numeric(0),
      point_y = numeric(0),
      point_z = numeric(0),
      direction_x = numeric(0),
      direction_y = numeric(0),
      direction_z = numeric(0),
      stringsAsFactors = FALSE
    )
    warning(
      "No muscle-force-direction rows were produced. Check exact body and muscle ",
      "names, then test one muscle in the default pose with output_frame_name = ",
      "'ground'."
    )
  } else {
    results <- do.call(rbind, result_rows)
    rownames(results) <- NULL
  }

  #write file
  if(write_file){
    write.csv(results, file = file_output, row.names = FALSE, na = "")
  }

  #return
  if(return_object){
    return(results)
  }

}
