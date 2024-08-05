#' Write a trc file
#'
#' Write a trc file
#' @param x dataframe containing data to write to trc. Should be in the same format produced from read.trc()
#' @param unit string contain unit of measurement in either "mm" or "m"
#' @param filename string identifying the filename to write to file. Must end in .trc
#' @return trc file of filename
#' @export

write.trc <- function()
{
    .Deprecated("write_trc")
    ## use new function, or remainder of myOldFunc
}
