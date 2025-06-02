#' Compute Hip Joint Center using Harrington's of Bell and Brand's Method
#'
#' This function computes the right and left hip joint centers based on Harrington's formula.
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
#' @param method A string specifying the method to use for calculations. Options are "harrington" or "bell". Default is "harrington".
#' @param append Logical. If TRUE, appends the computed hip joint center columns to the original data frame.
#'   If FALSE, returns a list containing the computed hip joint center data frames.
#' @return A data frame with appended hip joint center columns or a list of two data frames
#'   containing the right and left hip joint centers.
#' @references
#' Bell AL, Pederson DR, and Brand RA (1989) Prediction of hip joint center location from external landmarks. Human Movement Science. 8:3-16: Bell AL, Pedersen DR, Brand RA (1990) A Comparison of the Accuracy of Several hip Center Location Prediction Methods. J Biomech. 23, 617-621.
#' Harrington ME, Zavatsky AB, Lawson SE, Yuan Z, Theologis TN.(2007) Prediction of the hip joint centre in adults, children, and patients with cerebral palsy based on magnetic resonance imaging. J Biomech. 2007;40(3):595-602. Epub 2006 Apr 3.
#' @importFrom pracma cross
#' @importFrom dplyr select contains
#' @importFrom magrittr %>%
#' @export
compute_hjc_regression <- function(data,
                                   LASIS_name = "L.ASIS",
                                   RASIS_name = "R.ASIS",
                                   LPSIS_name = "L.PSIS",
                                   RPSIS_name = "R.PSIS",
                                   RHJC_name = "R.HJC",
                                   LHJC_name = "L.HJC",
                                   method = "harrington",
                                   append = TRUE) {

  # Load required libraries
  require(pracma)
  require(dplyr)
  require(magrittr)

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

    # Harrington or Bell formulae
    if (method == "harrington") {
      diff_ap <- -0.24 * PD - 9.9
      diff_v <- -0.30 * PW - 10.9
      diff_ml <- 0.33 * PW + 7.3
    } else if (method == "bell") {
      diff_ap <- -0.19 * PW
      diff_v <- -0.3 * PW
      diff_ml <- 0.36 * PW
    } else {
      stop("Invalid method specified. Choose either 'harrington' or 'bell'.")
    }

    vett_diff_pelvis_sx <- c(-diff_ml, diff_ap, diff_v, 1) #left
    vett_diff_pelvis_dx <- c(diff_ml, diff_ap, diff_v, 1) #right

    # HJC in pelvis CS (4x4)
    rhjc_pelvis <- OPB + vett_diff_pelvis_dx
    lhjc_pelvis <- OPB + vett_diff_pelvis_sx

    # Transformation Local to Global
    RHJC[, time_i] <- (pelvis[1:3, 1:3] %*% rhjc_pelvis[1:3]) + OB
    LHJC[, time_i] <- (pelvis[1:3, 1:3] %*% lhjc_pelvis[1:3]) + OB
  }

  # Transpose results to match MATLAB output format
  RHJC_df <- t(RHJC) %>% as.data.frame() %>% `colnames<-`(paste0(RHJC_name, "_", c("X", "Y", "Z")))
  LHJC_df <- t(LHJC)  %>% as.data.frame() %>% `colnames<-`(paste0(LHJC_name, "_", c("X", "Y", "Z")))

  # return results
  if (append) {
    return(cbind.data.frame(data, RHJC_df, LHJC_df))
  } else {
    return(list(RHJC = RHJC_df, LHJC = LHJC_df))
  }
}
