# Visualize Motion Using OpenSim

This function visualizes a motion file using the OpenSim command line
tool \`opensim-cmd\`.

## Usage

``` r
osim_viz_motion_cmd(
  opensim_path = "C:/\"OpenSim 4.5\"/bin",
  motion_file_path = "ik.mot",
  model_file_path = "Trunk_pelvis_LL.osim"
)
```

## Arguments

- opensim_path:

  A character string specifying the path to the OpenSim bin directory.

- motion_file_path:

  A character string specifying the path to the motion file (.mot or
  .sto).

- model_file_path:

  A character string specifying the path to the model file (.osim).

## Value

The result of running the OpenSim command.
