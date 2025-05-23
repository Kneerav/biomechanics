% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_muscle_colours_osim.R
\name{update_muscle_colours_osim}
\alias{update_muscle_colours_osim}
\title{Update OpenSim Muscle Colours}
\usage{
update_muscle_colours_osim(
  model_input_file = "Baseline_markers.osim",
  model_output_file = "Baseline_markers_coloured.osim",
  colours = c("#009E73", "#009E73"),
  muscles = c("bflh140_r", "bfsh140_r")
)
}
\arguments{
\item{model_input_file}{Path to the input OpenSim model (.osim file).}

\item{model_output_file}{Path to save the updated OpenSim model.}

\item{colours}{A vector of colour codes (in hex format) for the muscles to be updated.}

\item{muscles}{A vector of muscle names to be updated in the model.}
}
\value{
A .osim file with updated muscle colours.
}
\description{
This function updates the muscle colours of an OpenSim model based on the given inputs.
It reads in an OpenSim model, updates the muscle colours, and saves the model to a new file.
}
