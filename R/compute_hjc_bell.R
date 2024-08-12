#' Compute Hip Joint Center using Bell and Brand's Method
#'
#' This function computes the right and left hip joint centers based on Bell and Brand's formula.
#'
#' @param data A data frame containing marker trajectories with columns corresponding to 
#'   the names specified by the `LASIS_name`, `RASIS_name`, `LPSIS_name`, and `RPSIS_name` parameters.
#' @param LASIS_name A string specifying the column name or pattern for the left anterior superior 
#'   iliac spine markers in the data.
#' @param RASIS_name A string specifying the column name or pattern for the right anterior superior 
#'   iliac spine markers in the data.
#' @param LPSIS_name A string specifying the column name or pattern for the left posterior superior 
#'   iliac spine markers in the data.
#' @param RPSIS_name A string specifying the column name or pattern for the right posterior superior 
#'   iliac spine markers in the data.
#' @param RHJC_name A string specifying the prefix for the right hip joint center column names in the output.
#' @param LHJC_name A string specifying the prefix for the left hip joint center column names in the output.
#' @param append Logical. If TRUE, appends the computed hip joint center columns to the original data frame.
#'   If FALSE, returns a list containing the computed hip joint center data frames.
#' @return A data frame with appended hip joint center columns or a list of two data frames 
#'   containing the right and left hip joint centers.
#' @importFrom pracma cross
#' @importFrom dplyr select contains
#' @importFrom magrittr %>%
#' @importFrom stats solve
#' @export
compute_hjc_bell <- function(data, 
                                   LASIS_name = "L.ASIS",
                                   RASIS_name = "R.ASIS",
                                   LPSIS_name = "L.PSIS",
                                   RPSIS_name = "R.PSIS",
                                   RHJC_name = "R.HJC",
                                   LHJC_name = "L.HJC",
                                   append = TRUE) {
  
  # Load required libraries
  library(pracma)
  library(dplyr)
  library(magrittr)
  
  # Convert marker data to matrix and transpose as needed
  LASIS <- data %>% select(contains(LASIS_name)) %>% as.matrix() %>% t()
  RASIS <- data %>% select(contains(RASIS_name)) %>% as.matrix() %>% t()
  LPSIS <- data %>% select(contains(LPSIS_name)) %>% as.matrix() %>% t()
  RPSIS <- data %>% select(contains(RPSIS_name)) %>% as.matrix() %>% t()
  
  num_time_points <- ncol(RASIS)
  
  # Initialize arrays to store results
  RHJC <- matrix(0, nrow = 3, ncol = num_time_points)
  LHJC <- matrix(0, nrow = 3, ncol = num_time_points)
  
  for (time_i in 1:num_time_points) {
    # Right-handed Pelvis reference system definition
    SACRUM <- (RPSIS[, time_i] + LPSIS[, time_i]) / 2
    OP <- (LASIS[, time_i] + RASIS[, time_i]) / 2
    
    PROVV <- (RASIS[, time_i] - SACRUM) / sqrt(sum((RASIS[, time_i] - SACRUM)^2))
    IB <- (RASIS[, time_i] - LASIS[, time_i]) / sqrt(sum((RASIS[, time_i] - LASIS[, time_i])^2))
    
    KB <- pracma::cross(IB, PROVV)
    KB <- KB / sqrt(sum(KB^2))
    
    JB <- pracma::cross(KB, IB)
    JB <- JB / sqrt(sum(JB^2))
    
    OB <- OP
    
    # Construct pelvis transformation matrix (4x4)
    pelvis <- rbind(
      cbind(IB, JB, KB, OB),
      c(0, 0, 0, 1)
    )
    
    pelvis_inv <- solve(pelvis)
    
    OPB <- pelvis_inv %*% c(OB, 1)
    
    PW <- sqrt(sum((RASIS[, time_i] - LASIS[, time_i])^2))
    PD <- sqrt(sum((SACRUM - OP)^2))
    
    # Bell and Brand formulae (for right side)
    diff_ap <- -0.19*PW
    diff_v <- -0.3*PW
    diff_ml <- 0.36*PW
    
    vett_diff_pelvis_sx <- c(-diff_ml, diff_ap, diff_v, 1)
    vett_diff_pelvis_dx <- c(diff_ml, diff_ap, diff_v, 1)
    
    # HJC in pelvis CS (4x4)
    rhjc_pelvis <- OPB + vett_diff_pelvis_dx
    lhjc_pelvis <- OPB + vett_diff_pelvis_sx
    
    # Transformation Local to Global
    RHJC[, time_i] <- (pelvis[1:3, 1:3] %*% rhjc_pelvis[1:3]) + OB
    LHJC[, time_i] <- (pelvis[1:3, 1:3] %*% lhjc_pelvis[1:3]) + OB
  }
  
  # Transpose results
  RHJC_df <- t(RHJC) %>% as.data.frame() %>% `colnames<-`(paste0(RHJC_name, "_", c("X", "Y", "Z")))
  LHJC_df <- t(LHJC)  %>% as.data.frame() %>% `colnames<-`(paste0(LHJC_name, "_", c("X", "Y", "Z")))
  
  # return results
  if (append) {
    return(cbind.data.frame(data, RHJC_df, LHJC_df))
  } else {
    return(list(RHJC = RHJC_df, LHJC = LHJC_df))
  }
}