% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_side_osim.R
\name{replace_side_torque_osim}
\alias{replace_side_torque_osim}
\title{Replace Side Torque in OpenSim Model}
\usage{
replace_side_torque_osim(
  model_input_file = "Baseline_markers.osim",
  model_output_file = "Baseline_markers_removed.osim",
  side = "l"
)
}
\arguments{
\item{model_input_file}{A string representing the path to the input OpenSim model file. Default is "Baseline_markers.osim".}

\item{model_output_file}{A string representing the path to the output OpenSim model file. Default is "Baseline_markers_removed.osim".}

\item{side}{A string indicating which side (left or right) of the body to remove the actuators for. Default is "l" (left).}
}
\value{
The function returns nothing, but writes the modified OpenSim model to `model_output_file`.
}
\description{
This function removes actuators for a specified side (left or right) in an OpenSim model and
adds new coordinate actuators (torques) to the corresponding side for certain body parts (e.g., hip, knee, and ankle).
Locked joints are automatically skipped.
}
