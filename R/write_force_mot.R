#' Write GRF Data Frame to File
#'
#' This function writes a GRF (Ground Reaction Force) data frame to a file with a specific header format suitable for use in OpenSim.
#'
#' @param data A data frame containing GRF data. The first column should represent time.
#' @param filename A character string specifying the name of the file to which the data should be written. Include the extension .mot for compatability with OpenSim.
#' @importFrom caroline write.delim
#' @export
#' @examples
#' # Create a sample GRF data frame
#' df <- data.frame(time = 1:10, force = rnorm(10))
#' # Write the data frame to a file named "grf_data.mot"
#' write_force_mot(df, "grf_data.mot")
write_force_mot <- function(data, filename) {
  
  # Ensure caroline package is available
  if (!requireNamespace("caroline", quietly = TRUE)) {
    stop("The 'caroline' package is required but not installed. Please install it using install.packages('caroline').")
  }
  
  # Get number of rows and columns
  nRows <- nrow(data)
  nColumns <- ncol(data)
  
  # Get time range
  time.min <- data[1, 1]
  time.max <- data[nrow(data), 1]
  
  # Create header
  Header <- c(
    "GRF_file",
    "version=1",
    paste0("nRows=", nRows),
    paste0("nColumns=", nColumns),
    "inDegrees=no",
    paste0("range ", time.min, " ", time.max),
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
