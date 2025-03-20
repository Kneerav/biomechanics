This is a work in progress. Some of the functions have not been fully tested, and there are many others that will be added in the near future. If you'd like to use it (as is), installation instructions are below. 

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

IMPORTANT

So functions use the OpenSim api, and therefore require installation of reticulate package, and the opensim conda package in an appropriate environment. 

