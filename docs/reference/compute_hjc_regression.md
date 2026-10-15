# Compute Hip Joint Center using Harrington's or Bell and Brand's Method

This function computes the right and left hip joint centers based on
Harrington's formula.

## Usage

``` r
compute_hjc_regression(
  data,
  LASIS_name = "L.ASIS",
  RASIS_name = "R.ASIS",
  LPSIS_name = "L.PSIS",
  RPSIS_name = "R.PSIS",
  RHJC_name = "R.HJC",
  LHJC_name = "L.HJC",
  method = "harrington",
  append = TRUE
)
```

## Arguments

- data:

  A data frame containing marker trajectories with columns corresponding
  to the names specified by the \`LASIS_name\`, \`RASIS_name\`,
  \`LPSIS_name\`, and \`RPSIS_name\` parameters.

- LASIS_name:

  A string specifying the column name or pattern for the left anterior
  superior iliac spine markers in the data.

- RASIS_name:

  A string specifying the column name or pattern for the right anterior
  superior iliac spine markers in the data.

- LPSIS_name:

  A string specifying the column name or pattern for the left posterior
  superior iliac spine markers in the data.

- RPSIS_name:

  A string specifying the column name or pattern for the right posterior
  superior iliac spine markers in the data.

- RHJC_name:

  A string specifying the prefix for the right hip joint center column
  names in the output.

- LHJC_name:

  A string specifying the prefix for the left hip joint center column
  names in the output.

- method:

  A string specifying the method to use for calculations. Options are
  "harrington" or "bell". Default is "harrington".

- append:

  Logical. If TRUE, appends the computed hip joint center columns to the
  original data frame. If FALSE, returns a list containing the computed
  hip joint center data frames.

## Value

A data frame with appended hip joint center columns or a list of two
data frames containing the right and left hip joint centers.

## References

Bell AL, Pederson DR, and Brand RA (1989) Prediction of hip joint center
location from external landmarks. Human Movement Science. 8:3-16: Bell
AL, Pedersen DR, Brand RA (1990) A Comparison of the Accuracy of Several
hip Center Location Prediction Methods. J Biomech. 23, 617-621.
Harrington ME, Zavatsky AB, Lawson SE, Yuan Z, Theologis TN.(2007)
Prediction of the hip joint centre in adults, children, and patients
with cerebral palsy based on magnetic resonance imaging. J Biomech.
2007;40(3):595-602. Epub 2006 Apr 3.
