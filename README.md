This package provides functions to perform biomechanics analysis in the R programming language. 

# Install 

To install, use the following:

```{r}
#install the remotes package if you don't akready have it
install.packages("remotes")

#install the package
remotes::install_github("Kneerav/biomechanics")
```

Once installed, you can load the library and use the various functions.

```{r}
library(biomechanics)
```

# Full setup

Some functions use the OpenSim api, and therefore require installation of reticulate package, and the opensim conda package in an appropriate environment. Please see the vignettes for further information on how to do this. 

