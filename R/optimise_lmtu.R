#' Optimise Muscle–Tendon Parameters in an OpenSim Model
#'
#' This function optimises muscle–tendon unit (MTU) parameters
#' (optimal fiber length, tendon slack length, and maximal isometric force)
#' for a scaled OpenSim model relative to a reference model using the
#' Millard2012EquilibriumMuscle formulation.
#'
#' The optimisation adjusts MTU parameters based on subject–model scaling
#' differences in height and mass, and can optionally handle one-legged models.
#'
#' @param model_reference Character. Path to the reference `.osim` model.
#' @param model_scaled Character. Path to the scaled subject `.osim` model.
#' @param model_file_out Character. Output path for the optimised `.osim` file.
#' @param ref_rda Character. Path to `.rda` file storing or loading precomputed reference muscle quantities. If not existent, will be computed and written from the reference model.
#' @param interval Integer. Number of intervals to use for coordinate sampling.
#' @param oneLeg Logical. If `TRUE`, assumes only right-side muscles are present and mirrors optimised values to the left side.
#' @param scale_fmax Logical. If `TRUE`, will scale the max isometric force based on regression equations from Handsfield et al. Requires input for `heightSub` and `massSub` arguments.
#' @param heightSub Numeric. Subject height (m).
#' @param massSub Numeric. Subject mass (kg).
#' @param heightRef Numeric. Reference model height (m).
#' @param massRef Numeric. Reference model mass (kg).
#' @param return_object Logical. If `TRUE`, returns the optimised `opensim::Model` object.
#' @param write_file Logical. If `TRUE`, writes the optimised model to `model_file_out`.
#'
#' @references
#' Winby, C.R., Lloyd, D.G. Kirk, T.B. Evaluation of different analytical methods for subject-specific scaling of musculotendon parameters. Journal of Biomechanics. 41: 1682-1688, 2008.
#' Modenese L, Ceseracciu E, Reggiani M, Lloyd DG. Estimation of musculotendon parameters for scaled and subject specific musculoskeletal models using an optimization technique. J Biomech. 2016 Jan 25;49(2):141-8. doi: 10.1016/j.jbiomech.2015.11.006.
#' Rezaie, M. muscleOptimizer. https://github.com/mrrezaie/muscleOptimizer
#' Handsfield GG, Meyer CH, Hart JM, Abel MF, Blemker SS. Relationships of 35 lower limb muscles to height and body mass quantified using MRI. J Biomech. 2014 Feb 7;47(3):631-8. doi: 10.1016/j.jbiomech.2013.12.002.
#'
#' @return Invisibly returns the optimised OpenSim model object if `return_object = TRUE`.
#' @importFrom reticulate import
#' @importFrom nnls nnls
#' @export
optimise_lmtu <- function(model_reference = "../TestData/lai_modified_3x_strength.osim",
                          model_scaled ="../TestData/Model_SCALED.osim",
                          model_file_out = "../TestData/Model_SCALED_opt.osim",
                          ref_rda = "../TestData/test_mtu_optimiser/reference_values.rda",
                          interval  = 2,
                          oneLeg = TRUE,
                          scale_fmax = FALSE,
                          heightSub = 1.68,
                          massSub = 75.337,
                          heightRef = 1.68,
                          massRef = 75.337,
                          return_object = TRUE,
                          write_file = TRUE
){

  #get opensim package
  osim <- reticulate::import("opensim")

  #check for input files
  if (!file.exists(model_reference)) stop("Reference model not found: ", model_reference, call. = FALSE)
  if (!file.exists(model_scaled)) stop("Scaled model not found: ", model_scaled, call. = FALSE)


  #initialise the OpenSim model
  modelRef <- osim$Model(model_reference)
  modelSub <- osim$Model(model_scaled)

  #Initialize the system state
  state <- modelRef$initSystem()

  #names of muscles
  muscles <-  modelRef$getMuscles()
  n_muscles <- muscles$getSize()
  nameMuscles <- list()
  for(muscle_i in 1L:n_muscles){

    muscle_iL = muscle_i - 1L
    nameMuscles[[muscle_i]] <-  muscles$get(muscle_iL)$getName()

  }
  nameMuscles <- unlist(nameMuscles)

  #Initialize coordinate-muscle mappings
  coordinateMuscle <- list()
  coord_set <-  modelRef$getCoordinateSet()
  coord_set_n <-  coord_set$getSize()

  #loop through coordinates and link with muscles
  for (coord_i in 1L:coord_set_n) {

    coord_iL <-  coord_i - 1L

    #get the coordinate and its name
    coord_obj = coord_set$get(coord_iL)
    coordinate_name <- coord_obj$getName()

    #name the element of the list by the coordinate, creating a new list to be filled by muscles
    coordinateMuscle[[coordinate_name]] <- list()

    #test if coordinate of interest is locked or prescribed
    if (!coord_obj$get_locked() && coord_obj$getMotionType() != 3) {

      #list to store initial lengths
      length0 <- list()

      #loop through all muscle, find initial length
      for(i in 1L:n_muscles){

        muscle_iL = i - 1L
        length0[[i]] <-  muscles$get(muscle_iL)$getLength(state)

      }

      #finalise list of muscle lengths at anatomical position
      length0 = unlist(length0)

      #get the default, min, max and midpoint of the coord's range
      r0 <- coord_obj$getDefaultValue()
      r1 <- coord_obj$getRangeMin()
      r2 <- coord_obj$getRangeMax()
      r3 <- (r1 + r2) / 2
      r_vec = c(r1,r2,r3)

      #create matrix to store muscle lengths at different coordinate values
      lengths <- matrix(nrow = n_muscles, ncol = 3)

      #loop through the r_vec object
      for (r_j in 1:3) {

        #set the coord value and realise position
        coord_obj$setValue(state, r_vec[r_j])
        modelRef$realizePosition(state)

        #get lengths at new positions for all muscles
        for(muscle_i in 1L:n_muscles){
          muscle_iL = muscle_i - 1L
          lengths[muscle_i ,r_j] <-  muscles$get(muscle_iL)$getLength(state)
        }

      }

      #change length to mm, and get the differene to anatomical
      dl <- 1000 * (lengths - length0) #muscle length change in mm
      ok <- rowSums(abs(dl)) > 1e-1 #test if change is greater than very small value, indicate muscle spanning that coord


      #append muscles for given coord
      for (muscle_i_name in nameMuscles[ok]) {

        #add the coordinate if the muscle spans it
        coordinateMuscle[[coordinate_name]] <- append(coordinateMuscle[[coordinate_name]], muscle_i_name)

      }

      #reset to default value
      coord_obj$setValue(state, r0)
    }
  }

  #coordinateMuscle is named list, names are coords and within is list of muscles

  #create named list of muscles, where each element contains the joints they span
  muscleCoordinate <- list()
  for (coordinate in names(coordinateMuscle)) {

    #get the part of the list relavent to the coord
    ii <- coordinateMuscle[[coordinate]]

    #test if there are any muscles that span it
    if (length(ii) > 0) {

      #if so, loop through these muscles
      for (muscle in ii) {

        #check if muscle is not
        if (!(muscle %in% names(muscleCoordinate))) {
          muscleCoordinate[[muscle]] <- list()  #Create an empty list for the muscle if it doesn't exist
        }

        #Append coordinate to the muscle's list
        muscleCoordinate[[muscle]] <- c(muscleCoordinate[[muscle]], coordinate)
      }
    }
  }

  #keep only one leg's data if TRUE (usually should be)
  if (oneLeg) {
    for (i in nameMuscles) {
      if (grepl("_l$", i)) {  #Check if the muscle name ends with '_l'
        muscleCoordinate[[i]] <- NULL  #Remove muscle from muscleCoordinate
      }
    }
  }

  #get list of coordinate combinations, and muscles that span those
  sharedCoordinates <- list()
  for (i in names(muscleCoordinate)) {
    ii <- paste(muscleCoordinate[[i]], collapse = ";")  #Join the coordinates with semicolon
    if (!(ii %in% names(sharedCoordinates))) {
      sharedCoordinates[[ii]] <- list()  #Create an empty list for this coordinate combination if not exists
    }
    sharedCoordinates[[ii]] <- c(sharedCoordinates[[ii]], i)  #Append muscle name to the list
  }

  #create a sequence of numbers based on interval and the coord limits
  coordinateInterval <- list()
  for (i in names(coordinateMuscle)) {
    ii <- coordinateMuscle[[i]]
    if (length(ii) > 0) {
      coordinate <- modelRef$getCoordinateSet()$get(i) #get the relavent coordinate
      rangeMin <- coordinate$getRangeMin()  #Get the min value
      rangeMax <- coordinate$getRangeMax()  #Get the max value
      coordinateInterval[[i]] <- seq(rangeMin, rangeMax, length.out = interval)  #Generate sequence
    }
  }

  #function to get muscle quantities (muscle length, tendon length, normalised fibre length and pennation angle)
  getMuscleQuantities <- function(modelFile) {

    #get model and initialise state
    model <- osim$Model(modelFile)
    state <- model$initSystem()

    #Initialize an empty list
    muscleQuantities <- list()

    #loop through muscle names to create matrix for data storage
    for (muscle_i_name in names(muscleCoordinate)) {

      #get the muscle of interest
      ii <- muscleCoordinate[[muscle_i_name]]

      #nPose is the interval raised to the power of the length of ii
      nPose <- interval^length(ii)

      #Create an empty matrix with nPose rows and 4 columns
      muscleQuantities[[muscle_i_name]] <- matrix(nrow = nPose, ncol = 4)
    }

    #loop through shared coordinates
    for (sharedCoord_i in names(sharedCoordinates)) {

      #Split the coordinate string into a list of coordinates
      coordinates <- unlist(strsplit(sharedCoord_i, ";"))
      cat("\tCoordinates:", paste(coordinates, collapse = ", "), "\n")  #Print the coordinates
      cat("\t\tN muscles:", length(sharedCoordinates[[sharedCoord_i]]), "\n")  #Print the number of muscles

      #Generate all combinations of coordinate values from coordinateInterval
      temp <- lapply(coordinates, function(coord) coordinateInterval[[coord]])
      combine <- expand.grid(temp)  #This generates all combinations of coordinate values

      #set the coordinates
      for (jj in 1:nrow(combine)) {
        #Set coordinate values for each combination
        for (kk in 1:length(coordinates)) {
          model$getCoordinateSet()$get(coordinates[kk])$setValue(state, combine[jj, kk], enforceContraints = FALSE)
        }

        #assemble state and realise position
        model$assemble(state)
        model$realizePosition(state)

        #loop through muscle names
        for (nameMuscle in sharedCoordinates[[sharedCoord_i]]) {  #For each muscle sharing the coordinates
          muscle <- model$getMuscles()$get(nameMuscle)
          muscle$setActivation(state, 1)  #set activation to 1
          muscle$computeEquilibrium(state) #set equilibrium

          #Get muscle quantities
          quantities <- c(
            muscle$getLength(state),
            muscle$getNormalizedFiberLength(state),
            muscle$getTendonLength(state),
            muscle$getPennationAngle(state)
          )

          #Store quantities in muscleQuantities
          muscleQuantities[[nameMuscle]][jj, ] <- quantities
        }
      }

      #Reset coordinates to their default values after processing
      for (k in coordinates) {
        default <- model$getCoordinateSet()$get(k)$getDefaultValue()
        model$getCoordinateSet()$get(k)$setValue(state, default)
      }
    }

    #Return muscleQuantities after processing
    return(muscleQuantities)

  }


  #Load or calculate muscle quantities
  if (file.exists(ref_rda)) {
    #cat('Pre-calculated muscle quantities exist:\n\tload', ref_rda, '\n')
    message("Loading precomputed muscle reference data: ", ref_rda)
    load(ref_rda)
    print_rda <- FALSE
  } else {
    #cat('Reference model:', nameRef, '\n')
    ref <- getMuscleQuantities(modelRef)
    print_rda <- TRUE
  }

  #get scaled model quantities
  sub <- getMuscleQuantities(modelSub)

  #Initialize a list to store optimized results
  optimized <- list()

  #loop through muscle names
  for (muscle_i_name in names(muscleCoordinate)) {
    cat('\n', muscle_i_name, '\n')

    #get muscle, then quantities of interest
    muscleRef <- modelRef$getMuscles()$get(muscle_i_name)
    MIF <- muscleRef$getMaxIsometricForce()
    OFL <- muscleRef$getOptimalFiberLength()
    TSL <- muscleRef$getTendonSlackLength()
    PAO <- muscleRef$getPennationAngleAtOptimalFiberLength()

    ref[[muscle_i_name]] <- as.array(ref[[muscle_i_name]])  #Ensure it's a numeric array
    row <- nrow(ref[[muscle_i_name]])

    #Calculate tendon length limits
    limit <- sin(PAO) / sin(acos(0.1))
    if (limit < 0.5) limit <- 0.5
    ok <- ref[[muscle_i_name]][, 2] > limit

    #Extract muscle quantities from ref
    MTL <- ref[[muscle_i_name]][, 1][ok]  #muscle-tendon length
    NFL <- ref[[muscle_i_name]][, 2][ok]  #normalized fiber length
    TL  <- ref[[muscle_i_name]][, 3][ok]  #tendon length
    NTL <- TL / TSL           #normalized tendon length
    PA  <- ref[[muscle_i_name]][, 4][ok]  #pennation angle
    NFLT <- NFL * cos(PA)     #normalized fiber length along tendon

    #muscle-tendon length of scaled model
    MTL2 <- sub[[muscle_i_name]][, 1][ok]

    #Solve the least-squares equation Ax = b using nnls
    A <- cbind(NFLT, NTL)
    b <- MTL2
    x <- nnls::nnls(A, b)$x
    if (min(x) <= 0) {
      if (max(TL) - min(TL) < 1e-3) {
        cat('\tWARNING: no change in tendon length;\n\tRECOMPUTE ...\n')
      }
      fraction <- TL / MTL
      proportion <- fraction * MTL2  #tendon-muscle tendon length proportion

      A1 <- cbind(NFLT, rep(0, length(NTL)))  #normalized fiber length
      b1 <- MTL2 - proportion               #actual fiber length
      x1 <- nnls::nnls(A1, b1)$x[1]              #optimal fiber length

      A2 <- cbind(rep(0, length(NFLT)), NTL)  #normalized tendon length
      b2 <- MTL2 - NFLT * x1                #actual tendon length
      x2 <- nnls::nnls(A2, b2)$x[2]               #tendon slack length

      x <- c(x1, x2)
    }

    error <- sum((A %*% x - b)^2)  #sum of squared error

    #Scale the muscle parameters using volume and length scaling
    if(is.null(height)){
      volumeScale <- (91 * massRef + 588) / (91 * massSub + 588)
    } else{
    volumeScale <- (47.0 * massRef * heightRef + 1285.0) / (47.0 * massSub * heightSub + 1285.0)
    }
    lengthScale <- x[1] / OFL

    if(scale_fmax){
      x <- c(x, (volumeScale / lengthScale) * MIF)} else {
        x <- c(x, MIF)
      }


    #Print results for this muscle
    cat(sprintf("\tOFL   : %.6f;  %.4f %%\n", OFL, 100 * (OFL - x[1]) / OFL))
    cat(sprintf("\tTSL   : %.6f;  %.4f %%\n", TSL, 100 * (TSL - x[2]) / TSL))
    cat(sprintf("\tMIF   : %.3f;  %.4f %%\n", x[3], 100 * (MIF - x[3]) / MIF))

    optimized[[muscle_i_name]] <- x  #Store optimized results
  }

  #Update muscle parameters for the scaled model
  for (i in names(optimized)) {
    muscleSub <- modelSub$getMuscles()$get(i)
    muscleSub$setOptimalFiberLength(optimized[[i]][1])
    muscleSub$setTendonSlackLength(optimized[[i]][2])
    muscleSub$setMaxIsometricForce(optimized[[i]][3])
  }

  #Handle one-leg models
  if (oneLeg) {
    for (i in names(optimized)) {
      other <- paste0(substr(i, 1, nchar(i) - 2), "_l")
      muscleSub <- modelSub$getMuscles()$get(other)
      muscleSub$setOptimalFiberLength(optimized[[i]][1])
      muscleSub$setTendonSlackLength(optimized[[i]][2])
      muscleSub$setMaxIsometricForce(optimized[[i]][3])
      ref[[other]] <- ref[[i]]
    }
  }

  #Save the updated reference model to JSON
  if (print_rda) {
    save(ref, file = ref_rda)
  }

  #Export scaled model to OpenSim file
  if(write_file){
    modelSub$printToXML(model_file_out)
  }

  #return model if needed
  if (return_object){
    invisible(modelSub)
  } else invisible(NULL)

}
