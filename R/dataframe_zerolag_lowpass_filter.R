#' Filter Data Frame Columns Using a Zero-Lag Low-Pass Filter
#'
#' This function filters selected columns of a data frame using a low-pass Butterworth filter with padding. 
#' Non-filtered columns are preserved in their original positions.
#'
#' @param data A data frame containing the data to be filtered.
#' @param cut_off Numeric. The cut-off frequency for the low-pass filter in Hz.
#' @param sample_rate Numeric. The sample rate of the signal in Hz.
#' @param order Numeric. The order of the Butterworth filter.
#' @param pad_size Numeric. The number of data points to pad at the start and end of the signal.
#' @param exclude Numeric vector or character vector. Column indices or names to exclude from filtering.
#' @return A data frame with the filtered columns and original unfiltered columns in their original positions.
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr everything
#' @importFrom dplyr bind_cols
#' @importFrom dplyr as_tibble
#' @importFrom purrr map_dfc
#' @export
dataframe_zerolag_lowpass_filter <- function(data,
                                     cut_off = 6,
                                     sample_rate = 120,
                                     order = 2,
                                     pad_size = 500,
                                     exclude = NULL) {
  
  # Convert exclude to character if it's numeric
  if (is.numeric(exclude)) {
    exclude <- names(data)[exclude]
  }
  
  # Identify columns to be filtered and excluded
  all_cols <- names(data)
  filter_cols <- setdiff(all_cols, exclude)
  
  # Check if there are columns to filter
  if (length(filter_cols) == 0) {
    stop("No columns to filter. Ensure there are columns not excluded.")
  }
  
  # Apply the filter to each selected column
  filtered_cols <- data %>%
    dplyr::select(all_of(filter_cols)) %>%
    purrr::map_dfc(~zerolag_lowpass_filter(.x, cut_off, sample_rate, order, pad_size))
  
  # Combine filtered columns with the excluded columns
  result <- dplyr::bind_cols(
    filtered_cols,
    data %>% dplyr::select(all_of(exclude))
  )
  
  # return in original configuration
  return(result[,all_cols])
}
