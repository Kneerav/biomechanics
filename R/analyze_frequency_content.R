#' Analyze Frequency Content of a Data Frame
#'
#' Computes the frequency spectrum of numerical columns in a data frame
#' using the Fast Fourier Transform (via `seewave::spec()`).
#'
#' @param data A data frame containing the time-domain signal data.
#' @param exclude_cols Integer vector indicating which columns to exclude from analysis (e.g., time or label columns).
#' @param sample_rate Numeric. The sampling rate of the signals in Hz (default is 200 Hz).
#'
#' @return A data frame with frequency content for each included column:
#' \describe{
#'   \item{Frequencies_kHz}{Frequencies in kilohertz.}
#'   \item{Magnitude}{Magnitude of the frequency component.}
#'   \item{Frequencies}{Frequencies in Hz.}
#'   \item{id}{Column name from original data that this spectrum corresponds to.}
#' }
#'
#' @importFrom seewave spec
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @export
analyze_frequency_content = function(data,
                                     exclude_cols = c(1,2),
                                     sample_rate = 200){

  # Ensure the required packages are available
  if (!requireNamespace("seewave", quietly = TRUE)) {
    stop("The 'seewave' package is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required but not installed.")
  }

  # Validate input
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  data_cols <- data[, -exclude_cols, drop = FALSE]

  spectrum_storage = list()

  for(col_i in 1:ncol(data_cols)){

    data_i = data_cols[,col_i]

    spectrum_i = seewave::spec(wave = data_i,
                               f = sample_rate,
                               plot = FALSE) %>%

      #convert to df
      as.data.frame() %>%

      #rename the columns
      `colnames<-`(c("Frequencies_kHz", "Magnitude")) %>%

      #convert the kHz columns into a Hz column
      mutate(Frequencies = Frequencies_kHz * 1000,
             id = colnames(data_cols[col_i]))


    spectrum_storage[[col_i]] = spectrum_i


  }

  #create df
  spectrum_storage_df = dplyr::bind_rows(spectrum_storage)

  #return object
  return(spectrum_storage_df)

}

