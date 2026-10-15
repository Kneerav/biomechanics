# Compute Frame Orientation

This function computes frame orientation (global to local) based on two
input vectors.

## Usage

``` r
compute_frame_orientation(v1, v2, recompute, frameorder)
```

## Arguments

- v1:

  A numeric matrix or data frame where each row represents a vector.

- v2:

  A numeric matrix or data frame where each row represents a vector.

- recompute:

  An integer indicating whether to recompute the orientation:

  - 1: Recompute u1 as the cross product of u2 and u3.

  - 2: Recompute u2 as the cross product of u3 and u1.

- frameorder:

  A numeric vector of length 3 specifying the order of the frames.

## Value

A 3-dimensional array where each slice corresponds to a 3x3 orientation
matrix.

## Examples

``` r
v1 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
v2 <- matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3, byrow = TRUE)
recompute <- 1
frameorder <- c(1, 2, 3)
compute_frame_orientation(v1, v2, recompute, frameorder)
#> Loading required package: pracma
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    1    0
#> [2,]    0    0    1
#> [3,]    1    0    0
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    1
#> [2,]    1    0    0
#> [3,]    0    1    0
#> 
```
