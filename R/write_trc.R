#' Write TRC File from Data Frame
#'
#' This function writes a data frame to a TRC (marker trajectory) file format used in motion capture data.
#'
#' @param data A data frame containing marker data. The first two columns should be frame number and time, followed by marker data in X, Y, Z format.
#' @param unit A character string specifying the unit of measurement (e.g., "mm"). Default is "mm".
#' @param filename A character string specifying the path to the file to be written.
#' @importFrom caroline write.delim
#' @export
#' @examples
#' # Create a sample data frame with marker data
#' df <- data.frame(Frame = 1:10, Time = seq(0, 1, length.out = 10), 
#'                  Marker1_X = rnorm(10), Marker1_Y = rnorm(10), Marker1_Z = rnorm(10),
#'                  Marker2_X = rnorm(10), Marker2_Y = rnorm(10), Marker2_Z = rnorm(10))
#' # Write the data frame to a TRC file named "example.trc"
#' write_trc(df, unit = "mm", filename = "example.trc")
write_trc <- function(data, 
                      unit = "mm", 
                      filename) {
  
  # Ensure caroline package is available
  if (!requireNamespace("caroline", quietly = TRUE)) {
    stop("The 'caroline' package is required but not installed. Please install it using install.packages('caroline').")
  }
  
  # Validate input data
  if (ncol(data) < 3 || nrow(data) < 1) {
    stop("The data frame must have at least 3 columns and at least one row.")
  }
  
  # Extract the data frame 'y' as provided in the original function
  y <- data
  
  # Create header1
  header1 <- data.frame(V1 = "PathFileType", V2 = "4", V3 = "(X/Y/Z)", V4 = "Trial.trc")
  
  # Create header2
  data_rate <- round(1 / (data[2, 2] - data[1, 2]), 0)
  num_frames <- nrow(y)
  num_markers <- (ncol(y) - 2) / 3
  header2 <- data.frame(
    DataRate = data_rate,
    CameraRate = data_rate,
    NumFrames = num_frames,
    NumMarkers = num_markers,
    Units = unit,
    OrigDataRate = data_rate,
    OrigDataStartFrame = 1,
    OrigNumFrames = num_frames
  )
  
  # Prepare column names
  new_names <- gsub("_X", "", colnames(y))
  new_names[seq(4, length(new_names), by = 3)] <- ""
  new_names[seq(5, length(new_names), by = 3)] <- ""
  new_names[1] <- "Frame#"
  new_names[2] <- "Time"
  
  axis <- c("", "", rep(c("X", "Y", "Z"), num_markers))
  numeric <- 1:num_markers
  numeric_3 <- rep(numeric, each = 3)
  numbers <- c("", "", numeric_3)
  axis_final <- paste(axis, numbers, sep = "")
  
  y <- rbind(axis_final, y)
  colnames(y) <- new_names
  
  # Write the headers and data to the file
  caroline::write.delim(df = header1, file = filename, col.names = FALSE, row.names = FALSE, sep = "\t")
  caroline::write.delim(df = header2, file = filename, col.names = TRUE, row.names = FALSE, sep = "\t", append = TRUE)
  caroline::write.delim(df = y, file = filename, col.names = TRUE, row.names = FALSE, sep = "\t", append = TRUE)
}
