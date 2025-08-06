#' Add a 3D Reference Frame to a Plotly Object
#'
#' Adds visual reference axes (X, Y, Z) to an existing 3D `plotly` object.
#' Each axis is drawn from the specified origin using direction vectors from a 3x3 reference frame matrix.
#'
#' @param p A `plotly` object (typically created with `visualise_3D_marker_position()`).
#' @param reference_frame A 3x3 numeric matrix. Each column represents a unit direction vector for an axis (X, Y, Z).
#' @param origin A numeric vector of length 3 specifying the origin point of the reference frame.
#' @param scale_factor Numeric value indicating the length of the axes to draw (default is 100).
#'
#' @return A modified `plotly` object with reference frame axes added.
#'
#' @importFrom plotly add_trace
#' @export
visualise_3D_frame <- function(p, reference_frame, origin, scale_factor = 100) {

  if (!inherits(p, "plotly")) stop("Input `p` must be a plotly object.")

  if (is.null(reference_frame) || is.null(origin)) return(p)

  # Normalize reference frame axes
  e1 <- reference_frame[, 1] / sqrt(sum(reference_frame[, 1]^2))
  e2 <- reference_frame[, 2] / sqrt(sum(reference_frame[, 2]^2))
  e3 <- reference_frame[, 3] / sqrt(sum(reference_frame[, 3]^2))

  # Add reference frame axes as lines
  p <- p %>%
    add_trace(
      x = c(origin[1], origin[1] + scale_factor * e1[1]),
      y = c(origin[3], origin[3] + scale_factor * e1[3]),
      z = c(origin[2], origin[2] + scale_factor * e1[2]),
      type = 'scatter3d',
      mode = 'lines',
      line = list(color = 'red', width = 4),
      text=NA,
      name = 'X-axis'
    ) %>%
    add_trace(
      x = c(origin[1], origin[1] + scale_factor * e2[1]),
      y = c(origin[3], origin[3] + scale_factor * e2[3]),
      z = c(origin[2], origin[2] + scale_factor * e2[2]),
      type = 'scatter3d',
      mode = 'lines',
      line = list(color = 'green', width = 4),
      text=NA,
      name = 'Y-axis'
    ) %>%
    add_trace(
      x = c(origin[1], origin[1] + scale_factor * e3[1]),
      y = c(origin[3], origin[3] + scale_factor * e3[3]),
      z = c(origin[2], origin[2] + scale_factor * e3[2]),
      type = 'scatter3d',
      mode = 'lines',
      line = list(color = 'blue', width = 4),
      text=NA,
      name = 'Z-axis'
    )

  return(p)
}
