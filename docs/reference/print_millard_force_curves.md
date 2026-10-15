# Print Millard Muscle Force Curves to CSV

This function extracts and saves muscle force-related curves (passive,
active, force–velocity, and tendon force–length) from a
Millard2012EquilibriumMuscle model in OpenSim.

## Usage

``` r
print_millard_force_curves(
  model = "../TestData/test_mtu_optimiser/model_baseline.osim",
  muscles = c("soleus_r"),
  type = c("passive", "active", "velocity", "tendon", "all"),
  output_dir = getwd()
)
```

## Arguments

- model:

  Character. Path to the \`.osim\` model file.

- muscles:

  Character vector. Names of muscles to extract curves for.

- type:

  Character. Type of curve(s) to extract: one of \`"passive"\`,
  \`"active"\`, \`"velocity"\`, \`"tendon"\`, or \`"all"\`.

- output_dir:

  Character. Directory where CSV files will be saved. Defaults to the
  current working directory.

## Value

Invisibly returns \`NULL\`. CSV files are written to \`output_dir\`.

## References

Millard M, Uchida T, Seth A, Delp SL. Flexing computational muscle:
modeling and simulation of musculotendon dynamics. J Biomech Eng. 2013
Feb;135(2):021005. doi: 10.1115/1.4023390.
