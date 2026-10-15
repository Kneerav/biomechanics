# Analyze Knee Compartment Forces

This function calculates the medial and lateral forces at the knee based
on input force and moment data. It computes the forces using the
provided force and moment data and applies the Moore-Penrose
pseudo-inverse to solve for the forces in the medial and lateral
compartments.

## Usage

``` r
analyse_knee_compartment_forces(
  jr_data,
  fx_col = "walker_knee_r_on_tibia_r_in_tibia_r_fx",
  fy_col = "walker_knee_r_on_tibia_r_in_tibia_r_fy",
  fz_col = "walker_knee_r_on_tibia_r_in_tibia_r_fz",
  mx_col = "walker_knee_r_on_tibia_r_in_tibia_r_mx",
  my_col = "walker_knee_r_on_tibia_r_in_tibia_r_my",
  mz_col = "walker_knee_r_on_tibia_r_in_tibia_r_mz",
  r_med = c(0, 0, -0.02),
  r_lat = c(0, 0, 0.02),
  fmed_name = "fmed_r",
  flat_name = "flat_r",
  append = TRUE
)
```

## Arguments

- jr_data:

  A data frame containing joint reaction forces and moments.

- fx_col:

  Name of the column containing the x-component of the force.

- fy_col:

  Name of the column containing the y-component of the force.

- fz_col:

  Name of the column containing the z-component of the force.

- mx_col:

  Name of the column containing the x-component of the moment.

- my_col:

  Name of the column containing the y-component of the moment.

- mz_col:

  Name of the column containing the z-component of the moment.

- r_med:

  A numeric vector of length 3 representing the medial vector (default
  is \`c(0,0,-0.02)\`).

- r_lat:

  A numeric vector of length 3 representing the lateral vector (default
  is \`c(0,0, 0.02)\`).

- fmed_name:

  The prefix for the columns naming the medial forces (default is
  \`"fmed_r"\`).

- flat_name:

  The prefix for the columns naming the lateral forces (default is
  \`"flat_r"\`).

- append:

  Logical value indicating whether to append the calculated forces to
  the input data frame (\`TRUE\` by default).

## Value

A data frame containing the original input data along with the
calculated medial and lateral forces (if \`append = TRUE\`), or just the
calculated forces (if \`append = FALSE\`).
