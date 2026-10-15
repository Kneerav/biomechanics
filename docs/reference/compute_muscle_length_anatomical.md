# Compute Anatomical Muscle Lengths from an OpenSim Model

Extracts muscle-tendon unit (MTU) lengths from a static OpenSim model at
its anatomical (neutral) posture.

## Usage

``` r
compute_muscle_length_anatomical(
  model_file = "../TestData/Model_SCALED.osim",
  file_output = "muscle_length_anatomical.sto",
  write_file = FALSE,
  return_object = TRUE
)
```

## Arguments

- model_file:

  Path to the \`.osim\` model file (OpenSim 4.x format).

- file_output:

  Name of the output file to write muscle lengths (optional).

- write_file:

  Logical. If \`TRUE\`, writes output to a \`.sto\` file.

- return_object:

  Logical. If \`TRUE\`, returns a data frame of muscle lengths.

## Value

If \`return_object = TRUE\`, returns a data frame with muscle lengths at
time = 0. Otherwise, returns invisibly.
