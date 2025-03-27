#' Update OpenSim Muscle Colours
#'
#' This function updates the muscle colours of an OpenSim model based on the given inputs.
#' It reads in an OpenSim model, updates the muscle colours, and saves the model to a new file.
#'
#' @param model_input_file Path to the input OpenSim model (.osim file).
#' @param model_output_file Path to save the updated OpenSim model.
#' @param colours A vector of colour codes (in hex format) for the muscles to be updated.
#' @param muscles A vector of muscle names to be updated in the model.
#' @return A .osim file with updated muscle colours.
#' @export
update_muscle_colours_osim <- function(model_input_file = "Baseline_markers.osim",
                                       model_output_file = "Baseline_markers_coloured.osim",
                                       colours = c("#009E73", "#009E73"),
                                       muscles = c("bflh140_r", "bfsh140_r")){
  
  #get packages
  require(XML)
  require(dplyr)
  
  #Read model
  x = XML::xmlParse(model_input_file)
  
  for(i in 1:length(muscles)){
    
    #Convert colour to rgd
    update_col = col2rgb(colours[i]) %>% as.vector() 
    update_col_norm = update_col / max(update_col)
    
    #get muscle
    nodes_i = XML::getNodeSet(x, paste0("//Millard2012EquilibriumMuscle [@name ='", muscles[i], "']"))
    
    #update colour
    xmlValue(nodes_i[[1]][["GeometryPath"]][["Appearance"]][["color"]]) = as.character(paste(update_col_norm[1], update_col_norm[2], update_col_norm[3]))
    
  }
  
  #save to file
  cat(XML::saveXML(x,
                   indent = TRUE,
                   prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"),
      file=(model_output_file))
}