#' Compute Euclidean Average of Two 3D Vectors
#'
#' This function computes the Euclidean average of two 3D vectors for each row in the data frame, commonly used to find the joint centre (e.g., knee, ankle, etc.).
#'
#' @param data A data frame containing columns with 3D vectors.
#' @param marker_name_1 A string specifying the column name or pattern for the first 3D vector in the data frame.
#' @param marker_name_2 A string specifying the column name or pattern for the second 3D vector in the data frame.
#' @param jc_name A string specifying the prefix for the average vector (joint centre) column names in the output.
#' @param append Logical. If TRUE, appends the computed average vector as new columns to the original data frame.
#' @param append Logical. If TRUE, appends the computed average vector as new columns to the original data frame.
#' @return A data frame with the computed average vector either appended to the original data frame 
#'   or as a separate data frame depending on the `append` argument.
#' @importFrom dplyr select contains
#' @importFrom magrittr %>%
#' @export
compute_jc_mid <- function(data, 
                           marker_name_1,
                           marker_name_2,
                           jc_name = "jc",
                           append = TRUE) {
  
  # Load required libraries
  library(dplyr)
  library(magrittr)
  
  # Extract columns for the two markers
  marker1 <- data %>% select(contains(marker_name_1)) %>% as.matrix()
  marker2 <- data %>% select(contains(marker_name_2)) %>% as.matrix()
  
  # Ensure that the data has 3D vectors (i.e., 3 columns for each marker)
  if (ncol(marker1) != 3 || ncol(marker2) != 3) {
    stop("Both markers must have exactly 3 columns corresponding to the X, Y, and Z coordinates.")
  }
  
  # Compute the Euclidean average
  average_vectors <- (marker1 + marker2) / 2
  
  # Convert the average vectors to a data frame and name the columns
  average_df <- as.data.frame(average_vectors) %>%
    `colnames<-`(paste0(jc_name, "_", c("X", "Y", "Z")))
  
  # Return results
  if (append) {
    return(cbind.data.frame(data, average_df))
  } else {
    return(average_df)
  }
}
