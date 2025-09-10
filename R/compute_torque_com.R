#' Compute Torque at Center of Mass (COM)
#'
#' This function calculates the torque at the center of mass (COM) given the
#' position of the COM, the ground reaction force (GRF), and the center of pressure (COP).
#' The torque is computed as the cross product of the vector from the COM to the COP
#' and the GRF vector.Note that positive means counterclockwise rotation about a given axis
#' (when the axis is pointing to you).
#'
#' @param com_pos_vec A numeric vector of length 3 representing the position of the center of mass (COM) in 3D space (x, y, z).
#' @param grf_vec A numeric vector of length 3 representing the ground reaction force (GRF) in 3D space (x, y, z).
#' @param cop_vec A numeric vector of length 3 representing the position of the center of pressure (COP) in 3D space (x, y, z).
#'
#' @return A numeric vector of length 3 representing the torque (tx, ty, tz) about the COM in 3D space.
#' @importFrom pracma cross
#' @export
compute_torque_com <- function(com_pos_vec,
                              grf_vec,
                              cop_vec){

  #get r vector from point of rotation to point of force application (cop - com)
  r_vec = cop_vec - com_pos_vec

  #compute torque as cross product (r X F)
  torque_vec = pracma::cross(r_vec, grf_vec)
  names(torque_vec) <- c("tx", "ty", "tz")

  #return
  return(torque_vec)

}
