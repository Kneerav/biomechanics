#' Compute Joint Moments from Muscle Forces and Moment Arms
#'
#' This function multiplies muscle-tendon unit (MTU) forces by corresponding moment arms
#' to compute net joint moments. It can return a data frame and/or write to a `.sto` file.
#'
#' @param mtu_force_df A data frame of MTU forces (rows = time points, columns = muscles).
#' @param moment_arm_df A data frame of moment arms (same dimensions as `mtu_force_df`).
#' @param file_output Character. Output filename for the `.sto` file. Defaults to `"moment.sto"`.
#' @param write_file Logical. Whether to write the result to a `.sto` file. Defaults to `TRUE`.
#' @param return_object Logical. Whether to return the result as a data frame. Defaults to `FALSE`.
#'
#' @return If `return_object = TRUE`, returns a data frame of joint moments. Otherwise, returns `NULL` invisibly.
#' @export
compute_muscle_moment = function(mtu_force_df,
                          moment_arm_df,
                          file_output = "moment.sto",
                          write_file = TRUE,
                          return_object = FALSE){

  #match the force data
  mtu_force_df = mtu_force_df[,colnames(moment_arm_df)]

  #moments
  moment_df = mtu_force_df * moment_arm_df

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
