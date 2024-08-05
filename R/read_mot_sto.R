#' Read OpenSim MOT or STO File as Data Frame
#'
#' This function reads an OpenSim MOT or STO file into a data frame. The file is read after locating the line containing the word "time".
#'
#' @param filename A character string specifying the path to the file to be read.
#' @return A data frame containing the data from the file, starting from the line that includes the word "time".
#' @importFrom readr read_lines
#' @importFrom utils read.delim
#' @export
#' @examples
#' # Read a sample MOT or STO file
#' df <- read_mot_sto("example.mot")
read_mot_sto <- function(filename) {
  
  # Ensure readr package is available
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("The 'readr' package is required but not installed. Please install it using install.packages('readr').")
  }
  
  # Locate the line containing the word "time"
  line <- 0L
  input <- "start"
  while (!grepl("time", input, ignore.case = TRUE)) {
    line <- line + 1L
    input <- readr::read_lines(filename, skip = line - 1L, n_max = 1L)
  }
  
  # Read data, skipping lines until "time" is found
  data <- utils::read.delim(filename, skip = line - 1L)
  
  # Return the data frame
  return(data)
}
