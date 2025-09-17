#' Compute Joint Moments from Muscle Forces and Moment Arms
#'
#' This function multiplies muscle-tendon unit (MTU) forces by corresponding moment arms
#' to compute net joint moments. It can return a data frame and/or write to a `.sto` file.
#'
#' @param mtu_force_df A data frame of MTU forces (rows = time points, columns = muscles). Only columns that match column names for `moment_arm_df` are used.
#' @param moment_arm_df A data frame of moment arms (same number of rows as `mtu_force_df`).
#' @param exclude_cols A vector of column indices (or names) from `moment_arm_df` to exclude from the processing, to be re-binded at the end. Default to `1` to remove time column.
#' @param file_output Character. Output filename for the `.sto` file. Defaults to `"moment.sto"`.
#' @param write_file Logical. Whether to write the result to a `.sto` file. Defaults to `TRUE`.
#' @param return_object Logical. Whether to return the result as a data frame. Defaults to `FALSE`.
#'
#' @return If `return_object = TRUE`, returns a data frame of joint moments. Otherwise, returns `NULL` invisibly.
#' @export
analyse_muscle_moment = function(mtu_force_df,
                          moment_arm_df,
                          exclude_cols = 1,
                          file_output = "moment.sto",
                          write_file = TRUE,
                          return_object = FALSE){

  # Check if the two data frames have the same number of rows
  if (nrow(mtu_force_df) != nrow(moment_arm_df)) {
    stop("Error: The two data frames must have the same number of rows.")
  }

  # Exclude the specified columns
  excluded_data <- moment_arm_df[, exclude_cols, drop = FALSE]
  moment_arm_df <- moment_arm_df[, -exclude_cols, drop = FALSE]

  #match the force data
  mtu_force_df = mtu_force_df[,colnames(moment_arm_df)]

  #moments
  moment_df = mtu_force_df * moment_arm_df

  # Re-add the excluded columns (if any)
  if (ncol(excluded_data) > 0) {
    moment_df <- cbind(excluded_data, moment_df)
  }

  #return object
  if(return_object){
    return(moment_df)
  }

  #write file
  if(write_file){
    write_mot_sto(data = moment_df,
                                name = "moment",
                                inDegrees = "no",
                                filename = file_output)
  }

}
