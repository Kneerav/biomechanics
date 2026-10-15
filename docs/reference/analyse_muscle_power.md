# Analyse muscle power

This function computes muscle power as the product of muscle force and
muscle lengthening velocity. It includes an option to reverse the
velocity signal to treat shortening as positive (useful for power
generation interpretation).

## Usage

``` r
analyse_muscle_power(
  muscle_force_df,
  muscle_velocity_df,
  file_output = "muscle_power.sto",
  reverse_velocity = FALSE,
  write_file = TRUE,
  return_object = FALSE
)
```

## Arguments

- muscle_force_df:

  A dataframe containing the muscle force data (e.g., from a
  "muscle_forces.sto" file).

- muscle_velocity_df:

  A dataframe containing the muscle lengthening velocities (e.g., from a
  "muscle_velocities.sto" file).

- file_output:

  A string specifying the output file path for the muscle power results
  (e.g., "muscle_power.sto").

- reverse_velocity:

  A logical indicating whether to reverse the sign of velocity (default
  is FALSE).

- write_file:

  A logical indicating whether to write the results to a file (default
  is TRUE).

- return_object:

  A logical indicating whether to return the resulting data frame
  (default is FALSE).

## Value

If return_object = TRUE, returns a data frame with the computed muscle
power. If write_file = TRUE, writes the data to a file.
