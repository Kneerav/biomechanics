# Processing 3D marker data

``` r
library(biomechanics)
#> Warning: replacing previous import 'ggplot2::last_plot' by 'plotly::last_plot'
#> when loading 'biomechanics'
```

## Overview

Processing marker data involves the following steps:

- Rotating data so that positive x, y, and z axis point towards forward,
  up, and to the right, respectively
- Filtering the data (usually with a cut-off of 6-20Hz)
- Computing virtual markers
- Writing the marker data to a .trc file

Noting that filtering data is not really necessary for a static trial
(where the participant is not moving), and that virtual markers are not
necessary in all cases (particularly dynamic trials).

For the following example, we assume that we have extracted and clipped
the data (no rotations applied yet).

## Reading data

First, we read a .trc file.

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

#read in trc file, assuming that this is raw and not rotated
df = read_trc("./data/static_z_up.trc")
```

## Visualise

We can create a 3D visual for a specified frame. Here, we can see that
the orientation of the data appears incorrect. This is because motion
capture systems typically have a “Z-up” coordinate system, but OpenSim
expects a “Y-up”.

``` r
#quick visual to check
visualise_3D_marker_position(df, selected_frame = 1)
#> Loading required package: plotly
#> Loading required package: ggplot2
#> 
#> Attaching package: 'plotly'
#> The following object is masked from 'package:ggplot2':
#> 
#>     last_plot
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following object is masked from 'package:graphics':
#> 
#>     layout
```

## Rotate

We can rotate our data using the
[`rotate_markers()`](../reference/rotate_markers.md) function.

``` r
#now rotate to OpenSim coordinate system (if needed)
df_rotated = 
  
  #raw data
  df %>% 
  
  #rotate anti-clockwise around z axis
  rotate_markers(., angle_deg = 90, axis = "z") %>% 
  
  #rotate anti-clockwise around x axis
  rotate_markers(., angle_deg = 90, axis = "x")

#visualise to check correct rotation
visualise_3D_marker_position(df_rotated, selected_frame = 1)
```

The [`rotate_markers()`](../reference/rotate_markers.md) function takes
a data.frame of marker trajectories (from
[`read_trc()`](../reference/read_trc.md)) and rotates around specified
axes by a specified amount. For intuition around the rotation, first
recognise that a clockwise rotation around an axis that is pointing to
you would be negative. Therefore, rotating our data from positive y-axis
= forward (i.e., Vicon system) to positive x-axis = forward (i.e.,
OpenSim system) could be achieved in multiple ways, one being two
anti-clockwise rotations of 90 degrees, from the z- then the x-axis.

## Filter

We often filter our marker data to smooth it. This won’t make a big
difference in this case as it is a static trial, but the same function
([`filter_lowpass_pad_df()`](../reference/filter_lowpass_pad_df.md)) can
be used for any .trc data. Note that the functions also pads the signal
by a specified amount on either side prior to filtering, then removes
that padding after. This helps to remove artefact from entering your
data.

``` r
#now lets add filtering (won't change much since this is static)
df_filtered = 
  
  #get the rotated data
  df_rotated %>% 
  
  #filter with zero-lag 4th order Butterworth filter (cutoff = 6Hz)
  filter_lowpass_pad_df(., 
                        time_col = "Time",
                        exclude_cols = c(1, 2), #don't filter the Frame and Time data 
                               cut_off = 6, #in Hz
                               sample_rate = 200, #in Hz
                               order = 2, #zero-lag filter, so the order doubles to 4
                               pad_time = 0.5) #pad data by 500ms either side to avoid artefacts
```

Choosing an appropriate filter is another discussion, but for context, a
low-pass Butterworth filter with a cut-off of 6-15Hz is fairly typical.

## Joint centres

For static trials, we often want to compute joint centres. The following
demonstrates how to compute the hip joint centre using regression
equations (either “harrington” or “bell” can be specified), or a
mid-point method for the knee ankle.

``` r
#finally, let's compute joint centres
df_processed = 
  
  #get the filtered data
  df_filtered %>% 
  
  #compute hip joint centres
  compute_hjc_regression(.,
                         LASIS_name = "L.ASIS",
                         RASIS_name = "R.ASIS",
                         LPSIS_name = "L.PSIS",
                         RPSIS_name = "R.PSIS",
                         RHJC_name = "R.HJC",
                         LHJC_name = "L.HJC",
                         method = "harrington") %>%  #most accurate regression method
  
  #compute knee joint centres
  compute_jc_mid(., 
                 marker_name_1 = "L.Knee.Med", 
                 marker_name_2 = "L.Knee.Lat", 
                 jc_name = "L.Knee") %>% 
  compute_jc_mid(., 
                 marker_name_1 = "R.Knee.Med", 
                 marker_name_2 = "R.Knee.Lat", 
                 jc_name = "R.Knee") %>% 
  
  #compute ankle joint centres
  compute_jc_mid(., 
                 marker_name_1 = "L.Ankle.Med", 
                 marker_name_2 = "L.Ankle.Lat", 
                 jc_name = "L.Ankle") %>% 
  compute_jc_mid(., 
                 marker_name_1 = "R.Ankle.Med", 
                 marker_name_2 = "R.Ankle.Lat", 
                 jc_name = "R.Ankle") 
#> Loading required package: pracma
#> 
#> Attaching package: 'pracma'
#> The following objects are masked from 'package:magrittr':
#> 
#>     and, mod, or

#re-visualise, which should show the new joint centre markers
visualise_3D_marker_position(df_processed, selected_frame = 1)
```

This step is not needed for all trial types, but is important for static
trials that we use for scaling models.

## Write

Finally, we can write our processed marker data to file for subsequent
analysis.

``` r
#then write to file
write_trc(df_processed, unit = "mm", "./data/static_processed.trc")
```
