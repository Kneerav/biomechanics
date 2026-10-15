# Read OpenSim MOT or STO File as Data Frame

This function reads an OpenSim MOT or STO file into a data frame. The
file is read after locating the line containing the word "time".

## Usage

``` r
read_mot_sto(filename)
```

## Arguments

- filename:

  A character string specifying the path to the file to be read.

## Value

A data frame containing the data from the file, starting from the line
that includes the word "time".
