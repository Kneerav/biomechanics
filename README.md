This package provides functions to perform biomechanics analysis in the R programming language. 

# Basic installation 

Basic installation can be done via the `install_github()` function from the `remotes` package.

```{r}

#install the package
remotes::install_github("Kneerav/biomechanics")
```

Once installed, you can load the library and use the various functions.

```{r}
library(biomechanics)
```

Full documentation and vignettes are hosted here: https://kneerav.github.io/biomechanics/index.html

# Full functionality

Some functions use the OpenSim api (https://simtk.org/projects/opensim). To enable these, we need to setup and appropriate python environment, install the opensim conda package, and use the `reticulate` package to access them in R. Please see the setting up vignette(https://kneerav.github.io/biomechanics/articles/setting-up.html) for further information on how to do this. 