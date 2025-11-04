#' Update Muscle Strength in OpenSim Model Using Handsfield Method
#'
#' This function updates the muscle strength of a scaled OpenSim model based on a reference model using
#' the Handsfield scaling method. It calculates new muscle properties and can save the modified model
#' to a file or return the updated model object.
#'
#' @param model_scaled_file A string specifying the path to the scaled OpenSim model file (default is "Baseline_scaled.osim").
#' @param model_ref_file A string specifying the path to the reference OpenSim model file (default is "Baseline_markers.osim").
#' @param model_output_file A string specifying the path for the output updated OpenSim model file (default is "Model_updated.osim").
#' @param write_file A logical value indicating whether to write the updated model to a file (default is TRUE).
#' @param return_object A logical value indicating whether to return the updated model object (default is FALSE).
#' @param scale_factor A numeric value indicating the scaling factor for muscle volume.
#'
#' @references
#' Handsfield GG, Meyer CH, Hart JM, Abel MF, Blemker SS. Relationships of 35 lower limb muscles to height and body mass quantified using MRI. J Biomech. 2014 Feb 7;47(3):631-8. doi: 10.1016/j.jbiomech.2013.12.002.
#'
#' @return If `return_object` is TRUE, returns the modified OpenSim model object; otherwise, NULL.
#' @importFrom reticulate import
#' @export
strength_update_handsfield_osim = function(model_scaled_file = "Baseline_scaled.osim",
                                      model_ref_file = "Baseline_markers.osim",
                                      model_output_file = "Model_updated.osim",
                                      write_file = TRUE,
                                      return_object = FALSE,
                                      scale_factor){

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

  #scaled model
  Model1 = osim$Model(model_scaled_file)
  Model1$initSystem()

  #model to be updated (derived from scaled)
  Model2 = osim$Model(Model1)
  Model2$initSystem()
  Model2$setName('modelModified')

  #generic model (E.g., Raj model)
  Model3 = osim$Model(model_ref_file)
  Model3$initSystem()

  #gets muscles and their corresponding number. Should be the same for all.
  Muscles1 = Model1$getMuscles()
  nMuscles1 = Muscles1$getSize()
  Muscles2 = Model2$getMuscles()
  nMuscles2 = Muscles2$getSize()
  Muscles3 = Model3$getMuscles()
  nMuscles3 = Muscles3$getSize()

  #loop through muscles
  for(i in 1L:nMuscles1){ #check if last muscle obtained

    #get muscle frome ahc model
    scaledMuscle = Muscles1$get(i-1L);
    updMuscle = Muscles2$get(i-1L);
    refMuscle = Muscles3$get(i-1L);

    #get generic model data
    OFL_ref = refMuscle$get_optimal_fiber_length()
    MIF_ref = refMuscle$get_max_isometric_force()
    PCSA_ref = MIF_ref / 60 #MIF = PCSA * 60
    VOL_ref = PCSA_ref * (OFL_ref*100) #PCSA = MV / OFL (multiplied by 100 to get cm)

    #get scaled model data
    OFL_scaled = scaledMuscle$get_optimal_fiber_length()
    MIF_scaled = scaledMuscle$get_max_isometric_force()
    PCSA_scaled = MIF_scaled / 60 #MIF = PCSA * 60
    VOL_scaled = PCSA_scaled * (OFL_scaled*100) #PCSA = MV / OFL (multiplied by 100 to get cm)

    #updated data
    VOL_upd = VOL_ref * scale_factor #change the generic muscle volume to the new
    PCSA_upd = VOL_upd / (OFL_scaled*100) #dived by scaled OFL to get new PCSA
    MIF_upd = PCSA_upd * 60 #divide new PCSA by 60 to get new MIF

    #update the muscle
    updMuscle$setMaxIsometricForce(MIF_upd)
  }

  #write to file
  if(write_file == TRUE){
    Model2$printToXML(model_output_file)
  }

  #return object
  if(return_object == TRUE){
    return(Model2)
  }

}
