# Compute Muscle-Tendon Unit (MTU) Strain

Computes strain (as a decimal) from dynamic and reference muscle length
data using: \$\$strain = (dynamic length - reference length) / reference
length\$\$

## Usage

``` r
compute_strain(
  length_df_dynamic,
  length_df_reference,
  file_output = "muscle_strain.sto",
  write_file = TRUE,
  return_object = FALSE
)
```

## Arguments

- length_df_dynamic:

  A data frame containing dynamic muscle length data. Assumes first
  column is "time".

- length_df_reference:

  A single-row data frame containing reference muscle lengths. Assumes
  first column is "time".

- file_output:

  Character string specifying the output filename for the strain .sto
  file.

- write_file:

  Logical. If `TRUE`, writes the strain output to a .sto file using
  biomechanics package.

- return_object:

  Logical. If `TRUE`, returns the computed strain data frame.

## Value

Either a data frame (if `return_object = TRUE`), or invisibly returns
NULL.
