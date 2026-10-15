# Compute Derivative of Data

This function computes the derivative of data (e.g., angular position)
using either a smooth spline fit or numerical gradient.

## Usage

``` r
compute_derivative(x, y, method = c("spline", "gradient"), deriv = 1)
```

## Arguments

- x:

  A numeric vector representing the independent variable (e.g., time).

- y:

  A numeric vector representing the dependent variable (e.g., angular
  position).

- method:

  A character string specifying the method to compute the derivative.
  Options are "spline" for smooth spline fitting or "gradient" for
  numerical gradient using
  [`pracma::gradient`](https://rdrr.io/pkg/pracma/man/gradient.html).
  Default is "spline".

- deriv:

  An integer specifying the order of the derivative. Default is 1 for
  the first derivative. Higher values (e.g., 2) will return higher
  derivatives (second derivative, etc.).

## Value

A numeric vector representing the derivative (e.g., angular velocity or
acceleration).
