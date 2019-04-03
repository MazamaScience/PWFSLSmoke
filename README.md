---
title: "PWFSLSmoke"
pagetitle: PWFSLSmoke"
---

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/PWFSLSmoke)](https://cran.r-project.org/package=PWFSLSmoke)
[![Downloads](http://cranlogs.r-pkg.org/badges/PWFSLSmoke)](https://cran.r-project.org/package=PWFSLSmoke)
[![Build Status](https://travis-ci.org/MazamaScience/PWFSLSmoke.svg?branch=master)](https://travis-ci.org/MazamaScience/PWFSLSmoke)

# PWFSLSmoke R Package 

```
Utilities for Working with PM2.5 Air Quality Monitoring Data
```

## Background

The USFS Pacific Wildland Fire Sciences Lab [AirFire](https://www.airfire.org) 
team works to model wildland fire emissions and has created the BlueSky Modeling 
Framework. This system  integrates a wide collection of models along a smoke 
modeling pipeline (fire  information, fuel loadings, consumption modeling, 
emissions modeling, time rate of  emissions modeling, plume height estimations, 
and smoke trajectory and dispersion  modeling). The resulting model output has 
been integrated into many different smoke  prediction systems and scientific 
modeling efforts.

The **PWFSLSmoke** R package is being developed for PWFSL to help modelers and 
scientists more easily work with PM2.5 data from monitoring locations across 
North America.

The package makes it easier to obtain data, perform analyses and generate 
reports. It includes functionality to:

 * download and easily work with regulatory PM2.5 data from the EPA and AirNow
 * download and quality control raw monitoring data from AIRSIS and WRCC
 * convert between UTC and local timezones
 * apply various algorithms to the data (nowcast, rolling means, aggregation, etc.)
 * provide interactive timeseries and maps through RStudio’s Viewer pane
 * create a variety of publication ready maps and timeseries plots

## Installation

This package is designed to be used with [R](https://cran.r-project.org) 
(>= 3.3) and [RStudio](https://www.rstudio.com) so make sure you have those 
installed first.

Users will want to install the **devtools** package to have access to the latest 
version of the package from Github.

The following packages should be installed by typing the following at the 
RStudio console:

```
# Note that vignettes require knitr and rmarkdown
install.packages('knitr')
install.packages('rmarkdown')
install.packages('MazamaSpatialUtils')
devtools::install_github('MazamaScience/PWFSLSmoke', build_vignettes=TRUE)
```

Any work with spatial data, *e.g.* assigning countries, states and timezones, 
will require installation of required spatial datasets. To get these datasets 
you should type the following at the RStudio console:

```
library(MazamaSpatialUtils)
dir.create('~/Data/Spatial', recursive=TRUE)
setSpatialDataDir('~/Data/Spatial')
installSpatialData()
```

## Examples

Additional R Notebooks that demonstrate the functionality of the package can be 
found in the
[localNotebooks](https://github.com/MazamaScience/PWFSLSmoke/tree/master/localNotebooks)
directory on github. These notebooks are not part of the package because they 
require installation of the **MazamaSpatialUtils** datasets.

To run them you should:

 * make sure you have the proper spatial data installed in `~/Data/Spatial/`
 * make sure you have both the **knitr** and **rmarkdown** packages installed
 * download the `localNotebooks/` directory
 * open a notebook with RStudio
 * click the "Knit" or "Preview" button in RStudio

----

This R package was created by [Mazama Science](http://mazamascience.com) and is 
being  funded by the USFS 
[Pacific Wildland Fire Sciences Lab](https://www.fs.fed.us/pnw/pwfsl/).


