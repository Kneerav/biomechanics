#' Read TRC File into Data Frame
#'
#' This function reads a TRC (marker trajectory) file and returns it as a data frame. The function processes the header to extract marker names and adjusts column names accordingly.
#'
#' @param filename A character string specifying the path to the TRC file to be read.
#' @return A data frame with columns for frame number, time, and marker coordinates (X, Y, Z).
#' @importFrom utils read.delim
#' @importFrom stats na.omit
#' @export
#' @examples
#' # Read a sample TRC file
#' df <- read_trc("example.trc")
read_trc <- function(filename) {

  # Ensure utils package is available
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("The 'utils' package is required but not installed. Please install it using install.packages('utils').")
  }

  # Get the header information
  header <- utils::read.delim(filename, skip = 3, nrows = 1, header = FALSE)
  header <- header[seq(3, length(header), by = 3)]
  header <- na.omit(as.vector(t(header)))

  # Create plane-specific marker names
  header_x <- paste(header, "_X", sep = "")
  header_y <- paste(header, "_Y", sep = "")
  header_z <- paste(header, "_Z", sep = "")

  # Combine the final marker names
  final_header <- c(rbind(header_x, header_y, header_z))

  # Read the data
  datum <- utils::read.delim(filename, skip = 4)

  # Correct column names
  colnames(datum) <- c("Frame", "Time", final_header)

  #remove any columns with only NAs
  na_columns <- sapply(datum, function(col) all(is.na(col)))

  # Subset data frame to exclude columns that are all NA
  datum = datum[ , !na_columns]

  # Return the data frame
  return(datum)
}
