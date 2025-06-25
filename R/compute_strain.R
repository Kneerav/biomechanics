#function to compute MTU strain
#' Compute Muscle-Tendon Unit (MTU) Strain
#'
#' Computes strain (as a decimal) from dynamic and reference muscle length data using:
#' \deqn{strain = (dynamic length - reference length) / reference length}
#'
#' @param length_df_dynamic A data frame containing dynamic muscle length data. May include a "time" column.
#' @param length_df_reference A single-row data frame containing reference muscle lengths. May or may not include a "time" column.
#' @param file_output Character string specifying the output filename for the strain .sto file.
#' @param write_file Logical. If \code{TRUE}, writes the strain output to a .sto file using biomechanics package.
#' @param return_object Logical. If \code{TRUE}, returns the computed strain data frame.
#'
#' @return Either a data frame (if \code{return_object = TRUE}), or invisibly returns NULL.
#'
#' @export
compute_strain = function(length_df_dynamic,
                          length_df_reference,
                          file_output = "muscle_strain.sto",
                          write_file = TRUE,
                          return_object = FALSE){
  
  
  # Check dimensions match
  if (ncol(length_df_dynamic) != ncol(length_df_reference)) {
    stop("The number of columns in the muscle length files do not match.")
  }
  
  #check anatomical file has 1 row
  if(nrow(length_df_dynamic) != 1){
    stop("The reference muscle length file must contain exactly 1 row.")
  }
  
  # Check column names match
  if (!all(names(length_df_dynamic) == names(length_df_reference))) {
    stop("The column names of the dynamic and reference files do not match.")
  }
  
  #get time vector
  time_vector = length_df_dynamic$time
  length_df_reference_data = length_df_reference[,-1]
  length_df_dynamic_data = length_df_dynamic[,-1]
  
  # Compute strain (might have to loop through)
  reference_matrix <- matrix(rep(as.numeric(length_df_reference_data), each = nrow(length_df_dynamic_data)),
                             nrow = nrow(length_df_dynamic_data),
                             byrow = FALSE)
  
  strain_df <- (length_df_dynamic_data - reference_matrix) / reference_matrix
  colnames(strain_df) <- colnames(length_df_dynamic_data)
  
  # Add time column back if it existed
  strain_df <- cbind(time = time_vector, strain_df)
  
  strain_df <- as.data.frame(strain_df)
  
  #return object
  if(return_object){
    return(strain_df)
  }
  
  #write file
  if(write_file){
    write_mot_sto(data = strain_df, 
                  name = "strain", 
                  inDegrees = "no", 
                  filename = file_output)
  }
  
}