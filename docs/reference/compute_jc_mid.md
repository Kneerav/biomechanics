# Compute Euclidean Average of Two 3D Vectors

This function computes the Euclidean average of two 3D vectors for each
row in the data frame, commonly used to find the joint centre (e.g.,
knee, ankle, etc.).

## Usage

``` r
compute_jc_mid(
  data,
  marker_name_1,
  marker_name_2,
  jc_name = "jc",
  append = TRUE
)
```

## Arguments

- data:

  A data frame containing columns with 3D vectors.

- marker_name_1:

  A string specifying the column name or pattern for the first 3D vector
  in the data frame.

- marker_name_2:

  A string specifying the column name or pattern for the second 3D
  vector in the data frame.

- jc_name:

  A string specifying the prefix for the average vector (joint centre)
  column names in the output.

- append:

  Logical. If TRUE, appends the computed average vector as new columns
  to the original data frame.

## Value

A data frame with the computed average vector either appended to the
original data frame or as a separate data frame depending on the
\`append\` argument.
