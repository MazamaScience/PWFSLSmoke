# PWFSLSmoke R Package

```
Utilities for working with PM2.5 monitoring data available
from the AirNow, AIRSIS, WRCC and others.
```

## Background

The USFS Pacific Wildland Fire Sciences Lab [AirFire](https://www.airfire.org) team works
to model wildland fire emissions and has created the BlueSky Modeling Framework. This
system  integrates a wide collection of models along a smoke modeling pipeline (fire 
information, fuel loadings, consumption modeling, emissions modeling, time rate of 
emissions modeling, plume height estimations, and smoke trajectory and dispersion 
modeling). The resulting model output has been integrated into many different smoke 
prediction systems and scientific modeling efforts.

The **PWFSLSmoke** R package is being developed for PWFSL to help modelers and scientists
more easily work with PM2.5 data from monitoring locations across North America.

The package includes functionality to make it easier to:

 * download and work with monitoring data
 * create maps and timeseries plots of monitoring data
 * convert between local and GMT times

## Installation

This package is designed to be used with [R](https://cran.r-project.org) (>= 3.1.0) and
[RStudio](https://www.rstudio.com) so make sure you have those installed first.

Users will want to install the **devtools** package to have access to latest versions
of some packages that are not yet available on CRAN.

The following packages should be installed with devtools by typing the following at the RStudio console:

```
devtools::install_github('rstudio/dygraphs')
devtools::install_github('rstudio/leaflet')
devtools::install_github('mazamascience/MazamaSpatialUtils', build_vignettes=TRUE)
devtools::install_github('mazamascience/PWFSLSmoke', build_vignettes=TRUE)
```

Any work with spatial data, *e.g.* assigning counties or watersheds, will require installation of required
spatial datasets. To get these datasets you should execute the following commands from a shell terminal:

```
mkdir ~/Data
cd ~/Data
curl -O http://mazamascience.com/RData/Spatial.tar.gz
tar -xzf Spatial.tar.gz
```

## Examples

Working example scripts that demonstrate the functionality of the package can be found in the
[localExamples](https://github.com/MazamaScience/PWFSLSmoke/tree/master/localExamples)
directory on github. This directory is not part of the package.

----

This R package was created by [Mazama Science](http://mazamascience.com) and is being 
funded by the USFS [Pacific Wildland Fire Sciences Lab](https://www.fs.fed.us/pnw/pwfsl/).


