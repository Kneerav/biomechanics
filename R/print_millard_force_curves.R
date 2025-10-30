#' Print Millard Muscle Force Curves to CSV
#'
#' This function extracts and saves muscle force-related curves
#' (passive, active, force–velocity, and tendon force–length)
#' from a Millard2012EquilibriumMuscle model in OpenSim.
#'
#' @param model Character. Path to the `.osim` model file.
#' @param muscles Character vector. Names of muscles to extract curves for.
#' @param type Character. Type of curve(s) to extract:
#'   one of `"passive"`, `"active"`, `"velocity"`, `"tendon"`, or `"all"`.
#' @param output_dir Character. Directory where CSV files will be saved.
#'   Defaults to the current working directory.
#'
#' @references
#' Millard M, Uchida T, Seth A, Delp SL. Flexing computational muscle: modeling and simulation of musculotendon dynamics. J Biomech Eng. 2013 Feb;135(2):021005. doi: 10.1115/1.4023390.
#'
#' @return Invisibly returns `NULL`. CSV files are written to `output_dir`.
#' @importFrom reticulate import
#' @export
print_millard_force_curves <- function(model = "../TestData/test_mtu_optimiser/model_baseline.osim",
                                       muscles = c("soleus_r"),
                                       type = c("passive", "active", "velocity", "tendon", "all"),
                                       output_dir = getwd()){

  #match argument for safety (handles partial matching and errors)
  type <- match.arg(type)

  #get opensim package
  osim <- reticulate::import("opensim")

  #get model
  modelRef <- osim$Model(model)

  #loop through muscles.
  for(muscle_i in 1:length(muscles)){

    #get the muscle of interest
    force <- modelRef$getMuscles()$get(muscles[muscle_i]);
    myMuscle <- osim$Millard2012EquilibriumMuscle$safeDownCast(force)

    # === Passive ===
    if (type %in% c("passive", "all")) {
      message("  -> Writing passive curve")
      curve_passive <- myMuscle$get_FiberForceLengthCurve()
      curve_passive$printMuscleCurveToCSVFile(path = output_dir)
    }

    # === Active ===
    if (type %in% c("active", "all")) {
      message("  -> Writing active curve")
      curve_active <- myMuscle$get_ActiveForceLengthCurve()
      curve_active$printMuscleCurveToCSVFile(path = output_dir)
    }

    # === Velocity ===
    if (type %in% c("velocity", "all")) {
      message("  -> Writing force-velocity curve")
      curve_vel <- myMuscle$get_ForceVelocityCurve()
      curve_vel$printMuscleCurveToCSVFile(path = output_dir)
    }

    # === Tendon ===
    if (type %in% c("tendon", "all")) {
      message("  -> Writing tendon curve")
      curve_tendon <- myMuscle$get_TendonForceLengthCurve()
      curve_tendon$printMuscleCurveToCSVFile(path = output_dir)
    }
  }

  invisible(NULL)

}
