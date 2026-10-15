# Setting up for biomechanical analysis

## Installing

``` r
#install the remotes package if you don't akready have it
install.packages("remotes")

#install the package
remotes::install_github("Kneerav/biomechanics")
```

## Basic setup

To get going with the basic setup, you can just load your library.

``` r
library(biomechanics)
#> Warning: replacing previous import 'ggplot2::last_plot' by 'plotly::last_plot'
#> when loading 'biomechanics'
```

Please note that for full functionality, we also want the OpenSim api
set up.

## OpenSim api

Some functions depend on the OpenSim api
(<https://simtk.org/projects/opensim>). To set this up, we first need to
set up a Python environment.

Download and install either Anaconda or Miniconda from:
<https://www.anaconda.com/download>. I prefer Miniconda, as it is a
“lighter” version that takes up far less disk space than the full
Anaoconda version (but also does not include common packages).

Next, we want to create a new environment for the OpenSim conda package
to be installed. We try to use informative names, so in this instance it
will be called “opensim_scripting_452_311” with python version 3.11,
indicating OpenSim v4.5.2 and python version 3.11.

One way to achieve this is to open anaconda prompt and type the
following:

``` python
conda create -n "opensim_scripting_452_311" python=3.11
```

Alternatively, you can create the same environment by opening anaconda
navigator and create a new environment called
“opensim_scripting_452_311” with python version 3.11.

Once the environment is created, enter the code (in the conda prompt)
below to activate your new environment, install the opensim package. To
install a specific version (v4.5.2 is working well at the time of
writing this), we’d use the following:

``` python
conda activate opensim_scripting_452_311
conda install opensim-org::opensim==4.5.2
```

To install the latest version (which should be indicated here:
<https://anaconda.org/opensim-org/opensim>), do the following instead:

``` python
conda activate opensim_scripting_452_311
conda install opensim-org::opensim
```

Then switch to the python interpreter to test your installation.

``` python
python
import opensim as osim
```

If you have no errors, you are ready to roll. If you have errors, a
common issue solution I have observed is we sometimes need to downgrade
the OpenSSL package to 1.1.1w (which is most easily achieved in the
anaconda navigator).

Assuming the install works, you might as well install the additional
python packages we might need as follows.

``` python
exit() #to leave the python interpretor
conda install -c conda-forge btk #btk has numerous tools for working with c3d files
conda install scipy=1.16 #for signal processing 
pip install spm1d #statistical parametric mapping
```

Note that a specific version of `scipy` was specified to align with
OpenSim 4.5.2, since later versions are not compatible with the specific
version of `numpy` needed for the specific version of the `opensim`
package to work. By installing a specified version of `scipy`, we stop
this and the `spm1d` install process from automatically upgrading
`numpy` to an incompatible version. If you install another version of
OpenSim, you’ll have to be careful when installing other packages in the
same environment, particularly with `numpy` is a dependency.

Assuming that all went well, next open R Studio and ensure that the
`reticulate` package is setup to work with our new environment. The
easiest way I have found to do this is as follows:

``` r
library(reticulate) #you should have installed this package already
Sys.setenv(RETICULATE_PYTHON = "C:/Users/User/.conda/envs/opensim_scripting_452_311") #change file path as needed
py_discover_config() #check to see if reading correct version of python, should be 3.11
osim = import("opensim")
```

If no errors, you are good to go. Note that the above chunk of code
needs to be run every time you use any function requiring the
OpenSim-python api in a new R session (except `py_discover_congif()`).

## Other r packages

There are plenty of other R packages worth installing. Here is a few
basics:

``` r
install.packages(
  c("tidyverse", #you probably know why
    "readxl" #read excel files
    "pracma", #maths stuff
    "signal", #for signal processing
    "caroline", #some useful utility functions
    "fda", #functional data analysis
    )
  ) 
```

Note that `pracma` and `signal` are already dependencies for the
`biomechanics` package.

## Other recommended installs

I’d also recommend the following installs to make life easier:

- OpenSim GUI - Graphical user interface for OpenSim (musculoskeletal
  modelling software) <https://simtk.org/projects/opensim>
- Notepad ++ - text editor which is very useful for opening .xml and
  other files <https://notepad-plus-plus.org/>
- Mokka - Motion capture data visualiser
  <https://biomechanical-toolkit.github.io/mokka/>
