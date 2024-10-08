% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_frame_orientation_df.R
\name{compute_frame_orientation}
\alias{compute_frame_orientation}
\title{Compute Frame Orientation}
\usage{
compute_frame_orientation(v1, v2, recompute, frameorder)
}
\arguments{
\item{v1}{A numeric matrix or data frame where each row represents a vector.}

\item{v2}{A numeric matrix or data frame where each row represents a vector.}

\item{recompute}{An integer indicating whether to recompute the orientation:
\itemize{
  \item 1: Recompute u1 as the cross product of u2 and u3.
  \item 2: Recompute u2 as the cross product of u3 and u1.
}}

\item{frameorder}{A numeric vector of length 3 specifying the order of the frames.}
}
\value{
A 3-dimensional array where each slice corresponds to a 3x3 orientation matrix.
}
\description{
This function computes frame orientation based on two input vectors.
}
\examples{
v1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
v2 <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3, byrow = TRUE)
recompute <- 1
frameorder <- c(1, 2, 3)
compute_frame_orientation(v1, v2, recompute, frameorder)
}
