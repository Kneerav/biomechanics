# Optimise Muscle–Tendon Parameters in an OpenSim Model

This function optimises muscle–tendon unit (MTU) parameters (optimal
fiber length, tendon slack length, and maximal isometric force) for a
scaled OpenSim model relative to a reference model using the
Millard2012EquilibriumMuscle formulation.

## Usage

``` r
optimise_lmtu(
  model_reference = "../TestData/lai_modified_3x_strength.osim",
  model_scaled = "../TestData/Model_SCALED.osim",
  model_file_out = "../TestData/Model_SCALED_opt.osim",
  ref_rda = "../TestData/test_mtu_optimiser/reference_values.rda",
  interval = 2,
  oneLeg = TRUE,
  scale_fmax = FALSE,
  heightSub = 1.68,
  massSub = 75.337,
  heightRef = 1.68,
  massRef = 75.337,
  return_object = TRUE,
  write_file = TRUE
)
```

## Arguments

- model_reference:

  Character. Path to the reference \`.osim\` model.

- model_scaled:

  Character. Path to the scaled subject \`.osim\` model.

- model_file_out:

  Character. Output path for the optimised \`.osim\` file.

- ref_rda:

  Character. Path to \`.rda\` file storing or loading precomputed
  reference muscle quantities. If not existent, will be computed and
  written from the reference model.

- interval:

  Integer. Number of intervals to use for coordinate sampling.

- oneLeg:

  Logical. If \`TRUE\`, assumes only right-side muscles are present and
  mirrors optimised values to the left side.

- scale_fmax:

  Logical. If \`TRUE\`, will scale the max isometric force based on
  regression equations from Handsfield et al. Requires input for
  \`heightSub\` and \`massSub\` arguments.

- heightSub:

  Numeric. Subject height (m).

- massSub:

  Numeric. Subject mass (kg).

- heightRef:

  Numeric. Reference model height (m).

- massRef:

  Numeric. Reference model mass (kg).

- return_object:

  Logical. If \`TRUE\`, returns the optimised \`opensim::Model\` object.

- write_file:

  Logical. If \`TRUE\`, writes the optimised model to
  \`model_file_out\`.

## Value

Invisibly returns the optimised OpenSim model object if \`return_object
= TRUE\`.

## Details

The optimisation adjusts MTU parameters based on subject–model scaling
differences in height and mass, and can optionally handle one-legged
models.

## References

Winby, C.R., Lloyd, D.G. Kirk, T.B. Evaluation of different analytical
methods for subject-specific scaling of musculotendon parameters.
Journal of Biomechanics. 41: 1682-1688, 2008. Modenese L, Ceseracciu E,
Reggiani M, Lloyd DG. Estimation of musculotendon parameters for scaled
and subject specific musculoskeletal models using an optimization
technique. J Biomech. 2016 Jan 25;49(2):141-8. doi:
10.1016/j.jbiomech.2015.11.006. Rezaie, M. muscleOptimizer.
https://github.com/mrrezaie/muscleOptimizer Handsfield GG, Meyer CH,
Hart JM, Abel MF, Blemker SS. Relationships of 35 lower limb muscles to
height and body mass quantified using MRI. J Biomech. 2014 Feb
7;47(3):631-8. doi: 10.1016/j.jbiomech.2013.12.002.
