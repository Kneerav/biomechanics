# Compute Joint Moments from Muscle Forces and Moment Arms

This function multiplies muscle-tendon unit (MTU) forces by
corresponding moment arms to compute net joint moments. It can return a
data frame and/or write to a \`.sto\` file.

## Usage

``` r
compute_muscle_moment(
  mtu_force_df,
  moment_arm_df,
  file_output = "moment.sto",
  write_file = TRUE,
  return_object = FALSE
)
```

## Arguments

- mtu_force_df:

  A data frame of MTU forces (rows = time points, columns = muscles).

- moment_arm_df:

  A data frame of moment arms (same dimensions as \`mtu_force_df\`).

- file_output:

  Character. Output filename for the \`.sto\` file. Defaults to
  \`"moment.sto"\`.

- write_file:

  Logical. Whether to write the result to a \`.sto\` file. Defaults to
  \`TRUE\`.

- return_object:

  Logical. Whether to return the result as a data frame. Defaults to
  \`FALSE\`.

## Value

If \`return_object = TRUE\`, returns a data frame of joint moments.
Otherwise, returns \`NULL\` invisibly.
