#' Write Data Frame to OpenSim MOT or STO File
#'
#' This function writes a data frame to an OpenSim MOT or STO file with a specific header format.
#'
#' @param data A data frame containing the data to be written. Each column represents a variable, where the first column should represent time..
#' @param name A character string specifying the name of the file. Default is "Coordinates".
#' @param inDegrees A character string specifying whether the data is in degrees. Default is "no". Use "yes" for degrees.
#' @param filename A character string specifying the name of the file to which the data should be written. Include the extension .mot or .sto for compatability with OpenSim.
#' @importFrom caroline write.delim
#' @export
#' @examples
#' # Create a sample data frame
#' df <- data.frame(time = 1:10, value = sin(1:10))
#' # Write the data frame to a file named "example.mot"
#' write_mot_sto(df, name = "ExampleData", inDegrees = "no", filename = "example.mot")
write_mot_sto <- function(data, 
                          name = "Coordinates",
                          inDegrees = "no",
                          filename) {
  
  # Ensure caroline package is available
  if (!requireNamespace("caroline", quietly = TRUE)) {
    stop("The 'caroline' package is required but not installed. Please install it using install.packages('caroline').")
  }
  
  # Get number of rows and columns
  nRows <- nrow(data)
  nColumns <- ncol(data)
  
  # Create header
  Header <- c(
    name,
    "version=1",
    paste0("nRows=", nRows),
    paste0("nColumns=", nColumns),
    paste0("inDegrees=", inDegrees),
    "",
    "endheader"
  )
  
  # Write header to file
  caroline::write.delim(
    df = data.frame(Header),
    file = filename,
    col.names = FALSE,
    row.names = FALSE,
    sep = "\t"
  )
  
  # Write data underneath in the same file
  caroline::write.delim(
    df = data,
    file = filename,
    col.names = TRUE,
    row.names = FALSE,
    sep = "\t",
    append = TRUE
  )
}
