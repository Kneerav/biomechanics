% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prescribe_motion_osim.R
\name{prescribe_motion_osim}
\alias{prescribe_motion_osim}
\title{Prescribe Motion to OpenSim Model}
\usage{
prescribe_motion_osim(
  model_input_file = "model_file.osim",
  model_output_file = "model_prescribed.osim",
  IK_file = "IK.mot",
  low_pass_cutoff = 6,
  write_file = TRUE,
  return_object = FALSE
)
}
\arguments{
\item{model_input_file}{A string specifying the path to the input OpenSim model file (default is "model_file.osim").}

\item{model_output_file}{A string specifying the path for the output updated OpenSim model file (default is "model_prescribed.osim").}

\item{IK_file}{A string specifying the path to the IK data file (default is "IK.mot").}

\item{low_pass_cutoff}{A numeric value specifying the cutoff frequency for the low-pass filter (default is 6). Will be ignored if this is not a numeric value >= 1.}

\item{write_file}{A logical value indicating whether to write the updated model to a file (default is TRUE).}

\item{return_object}{A logical value indicating whether to return the updated model object (default is FALSE).}
}
\value{
If `return_object` is TRUE, returns the modified OpenSim model object; otherwise, NULL.
}
\description{
This function prescribes motion to the coordinates of an OpenSim model based on inverse kinematics (IK) data.
It can save the modified model to a file or return the updated model object. Note that the input model
must already have any locked joints welded prior to using this function.
}
