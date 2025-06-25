#' Compute Anatomical Muscle Lengths from an OpenSim Model
#'
#' Extracts muscle-tendon unit (MTU) lengths from a static OpenSim model at its anatomical (neutral) posture.
#'
#' @param model_file Path to the `.osim` model file (OpenSim 4.x format).
#' @param file_output Name of the output file to write muscle lengths (optional).
#' @param write_file Logical. If `TRUE`, writes output to a `.sto` file.
#' @param return_object Logical. If `TRUE`, returns a data frame of muscle lengths.
#'
#' @return If `return_object = TRUE`, returns a data frame with muscle lengths at time = 0.
#'         Otherwise, returns invisibly.
#'
#' @importFrom reticulate import
#' @export
compute_muscle_length_anatomical = function(model_file = "../TestData/Model_SCALED.osim",
                                            file_output = "muscle_length_anatomical.sto",
                                            write_file = FALSE,
                                            return_object = TRUE){
  
  #load packages
  require(dplyr)
  require(reticulate)
  
  #get opensim package
  osim = import("opensim")
  
  #import model
  model = osim$Model(model_file)
  model$initSystem()
  
  #get coords
  coord_set = model$getCoordinateSet()
  muscle_set = model$getMuscles()
  
  #sizes
  numCoords = coord_set$getSize()
  numMuscles = muscle_set$getSize()
  
  #storage prep
  coord_name_list = list()
  coord_traj_list = list()
  
  #re-initialise state
  state = model$initSystem()
  
  #muscle length list
  muscle_lengths = list()
  muscle_names = list()
  
  #remove actuator
  for(i in 1L:numMuscles){
    
    #adjust iterator
    i_L = i - 1L
    
    #get coord and muscle of interest
    muscle_i = muscle_set$get(i_L)
    
    #get current muscle info
    muscle_names[[i]] = muscle_i$getName()
    muscle_lengths[[i]] = muscle_i$getLength(state)
    
  }
  
  #bind too df
  muscle_lengths_df = data.frame(time = 0, do.call("cbind.data.frame", muscle_lengths) %>% 
                                   `colnames<-`(muscle_names)) 
  
  # Write to file
  if(write_file == TRUE){
    write_mot_sto(muscle_lengths_df, 
                  name = "length", 
                  inDegrees = "no",
                  file_output)
  }
  
  # Return object
  if(return_object == TRUE){
    return(muscle_lengths_df)
  }
  
  
}