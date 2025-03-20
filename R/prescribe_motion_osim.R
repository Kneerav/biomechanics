#' Prescribe Motion to OpenSim Model
#'
#' This function prescribes motion to the coordinates of an OpenSim model based on inverse kinematics (IK) data.
#' It can save the modified model to a file or return the updated model object. Note that the input model
#' must already have any locked joints welded prior to using this function.
#'
#' @param model_input_file A string specifying the path to the input OpenSim model file (default is "model_file.osim").
#' @param model_output_file A string specifying the path for the output updated OpenSim model file (default is "model_prescribed.osim").
#' @param IK_file A string specifying the path to the IK data file (default is "IK.mot").
#' @param low_pass_cutoff A numeric value specifying the cutoff frequency for the low-pass filter (default is 6). Will be ignored if this is not a numeric value >= 1.
#' @param write_file A logical value indicating whether to write the updated model to a file (default is TRUE).
#' @param return_object A logical value indicating whether to return the updated model object (default is FALSE).
#'
#' @return If `return_object` is TRUE, returns the modified OpenSim model object; otherwise, NULL.
#' @importFrom reticulate import
#' @importFrom stringr str_replace str_detect
#' @export
prescribe_motion_osim = function(model_input_file = "model_file.osim",
                                   model_output_file = "model_prescribed.osim",
                                   IK_file = "IK.mot",
                                   low_pass_cutoff = 6,
                                 write_file = TRUE,
                                 return_object = FALSE){


  #note that locked joint must be welded prior to using function

  # Check if reticulate is installed
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is not installed. Please install it to use this function.")
  }

  # Load OpenSim via reticulate
  osim <- NULL
  tryCatch({
    osim <- reticulate::import("opensim")
  }, error = function(e) {
    stop("Failed to import 'opensim' module. Please ensure that the OpenSim Python package is installed and accessible.")
  })

  # read mot file
  IK_data = read_mot_sto(IK_file)

  # get model
  osimModel=osim$Model(model_input_file)

  # get ik data as table
  TableProcessor = osim$TableProcessor(IK_file)

  # filter data
  if (is.numeric(low_pass_cutoff) && low_pass_cutoff >= 1) {
    TableProcessor$append(osim$TabOpLowPassFilter(low_pass_cutoff))
  }

  # process data
  coordinates = TableProcessor$process(osimModel)
  coordinates$trim(IK_data$time[1], IK_data$time[nrow(IK_data)])

  # write filtered data
  osim$STOFileAdapter_write(coordinates, stringr::str_replace(IK_file, "IK.mot", "IK_filtered.mot"))
  coordinateSto=osim$Storage(stringr::str_replace(IK_file, "IK.mot", "IK_filtered.mot")); #not robust

  # set new model name
  osimModel$setName('modelWithPrescribedMotion');

  # get coord info
  modelCoordSet = osimModel$getCoordinateSet();
  nCoords = modelCoordSet$getSize();

  for(k in 1L:nCoords){

    #set up arrays
    Time=osim$ArrayDouble();
    coordvalue = osim$ArrayDouble();

    #get current coordinate in the loop
    currentcoord = modelCoordSet$get(k-1L);

    # Get the Time stamps and Coordinate values
    coordinateSto$getTimeColumn(Time);
    coordinateSto$getDataColumn(currentcoord$getName(), coordvalue)

    if(stringr::str_detect(currentcoord$getName(), "mtp|wrist|beta")) next

    # Check if it is a rotational or translational coordinate
    motion = currentcoord$getMotionType()

    # construct a SimmSpline object (previously NaturalCubicSpline)
    Spline = osim$SimmSpline();

    # Now to write Time and coordvalue to Spline
    if(motion==1L){
      for(j in 1L:coordvalue$getSize()){
        Spline$addPoint(Time$getitem(j-1L),coordvalue$getitem(j-1L)/(180/pi))
      }
    } else {
      for(j in 1L:coordvalue$getSize()){
        Spline$addPoint(Time$getitem(j-1L),coordvalue$getitem(j-1L))
      }

    }

    # Add SimmSpline to the PrescribedFunction of the Coordinate being edited
    currentcoord$setPrescribedFunction(Spline)
    currentcoord$setDefaultIsPrescribed(TRUE)
  }

  # write to file
  if(write_file == TRUE){
    osimModel$printToXML(model_output_file)
  }

  # return object
  if(return_object == TRUE){
    return(osimModel)
  }
  # Return NULL invisibly if neither write nor return is requested
  invisible(NULL)

}
