# Rotate an Opensim Data Table

This function rotates a specified OpenSim data table around a given axis
by a specified angle in degrees.

## Usage

``` r
rotate_osim_table(table_object, axis = c(1, 0, 0), deg = -90)
```

## Arguments

- table_object:

  An object representing an OpenSim data table.

- axis:

  A numeric vector of length 3 specifying the rotation axis (default is
  c(1, 0, 0)).

- deg:

  A numeric value specifying the rotation angle in degrees (default is
  -90).

## Value

NULL. The function modifies the input \`table_object\` in place.
