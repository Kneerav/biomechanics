#' Filter Markers with Padding to Avoid Artifacts
#'
#' This function filters the marker data using a low-pass Butterworth filter with padding at the start and end of the data to avoid artifacts.
#'
#' @param data A data frame containing marker data to be filtered.
#' @param time_col A string indicating the name of the column containing the time vector.
#' @param exclude_cols A vector of column indices (or names) to exclude from filtering. Usually 1 and 2 to remove Time and Frame. 
#' @param cut_off Numeric. The cut-off frequency for the low-pass filter in Hz.
#' @param sample_rate Numeric. The sample rate of the data in Hz.
#' @param order Numeric. The order of the Butterworth filter.
#' @param pad_time Numeric. The time length to pad the data at the start and end.
#' @param use_spline_padding Logical. Whether to use spline interpolation for padding (TRUE) or replicate the first/last rows (FALSE). Default is FALSE.
#' @return A data frame with the filtered marker data, with padding removed.
#' @importFrom dplyr select
#' @importFrom purrr map_df
#' @importFrom signal butter filtfilt
#' @importFrom stats smooth.spline
#' @export
filter_lowpass_pad_df <- function(data,
                               time_col = "Time",
                               exclude_cols = c(1, 2),  
                               cut_off = 6,
                               sample_rate = 200,
                               order = 2,
                               pad_time = 0.5) {
  
  #Extract the non-marker columns based on user input
  non_marker_cols <- data[, exclude_cols, drop = FALSE]
  
  #Fit splines to each column of the data (excluding non-marker columns)
  padded_data <- list()
  
  for (i in 1:ncol(data)) {
    
    # Fit a spline to the column
    spline_fit <- smooth.spline(data[,time_col], data[,i])
    
    # Extrapolate beyond the first and last data points to create padding
    padded_data[[i]] <- predict(spline_fit, x = seq(from = data[1,time_col] - pad_time,
                                                    to = data[nrow(data),time_col] + pad_time,
                                                    by = 1/sample_rate))$y
    
    
  }
  
  #bind and rename data
  padded_df = do.call("cbind.data.frame", padded_data)
  colnames(padded_df) = colnames(data)
  
  # Filter the data
  bw <- signal::butter(n = order, W = cut_off / (sample_rate / 2), plane = "z", type = "low")
  
  # run the filter on all data columns
  filtered_padded <- 
    cbind.data.frame(
      
      padded_df %>% select(all_of(exclude_cols)),
      
      padded_df %>% 
        dplyr::select(-all_of(exclude_cols)) %>%
        purrr::map_df(~signal::filtfilt(bw, x = .x))
    )
  
  
  # remove padding
  filtered_data <- filtered_padded %>%  filter(Time > data$Time[1] - 0.00001, 
                                               Time <= data$Time[nrow(data)] + 0.00001 )
  
  #return filtered data
  return(filtered_data)
  
}
