# PWFSLSmoke 1.2.117

Documentation and other fixes to deal with CRAN check issues.

* Put all internet connected examples inside a `try({ ... })` block to fail
gracefully when resources are not available.
* Updated many examples to use the `Camp_Fire` dataset.
* Updated the "Introduction to PWFSLSmoke" vignette to use package data rather
than downloading data from the internet.
* Added `Camp_Fire` dataset.
* Moved dataset documentation to separate `data.R` file.

# PWFSLSmoke 1.2.116

* Support for parsing AIRSIS "EBAM MULTI2_B" format.

# PWFSLSmoke 1.2.115

* Fixed a cropping issue in `staticmap_getEsrimapBrick()` which resulted in
image boundaries that didn't exactly match the requested boundaries.

# PWFSLSmoke 1.2.114

* Updated all EBAM QC `valid_RHi` thresholds from 45 -> 50.

# PWFSLSmoke 1.2.113

* Updated `docker/` files to use R 3.6.3.

# PWFSLSmoke 1.2.112

* Updated support for parsing AIRSIS "ESAM Multi" format.

# PWFSLSmoke 1.2.111

CRAN manual review suggestions:

* Use `find.package()` instead of the much slower `installed.packages()` to
determine if a package is installed.
* When using `par()` to set graphical parameters, always use save the previously
set parameters and then `on.exit(par(opar))` as the very next line.
* Ensure that `tempdir()` or `tempfile()` are used in all examples, tests and
vignettes so that no files are written to user directories.
* Changed many examples from `\dontrun{}` to `\donttest{}`.

# PWFSLSmoke 1.2.110

* Tweaks for CRAN submission.

# PWFSLSmoke 1.2.109

* Removed logging setup from `initializeMazamaSpatialutils()`.

# PWFSLSmoke 1.2.108

* Added support for parsing AIRSIS "ESAM Multi" format.

# PWFSLSmoke 1.2.107

* Updated `test-timeinfo.R` to avoid CRAN Check errors.

# PWFSLSmoke 1.2.106

* Added support for parsing AIRSIS "EBAM Plus Multi" format.

# PWFSLSmoke 1.2.105

* Documentation tweaks.
* Removed `app/` directory with example web service.
* Updated vignettes.
* New vignette: "Example: Save Data as CSV"

# PWFSLSmoke 1.2.104

* Added support for parsing AIRSIS "EBAM Multi 2" format.
* Replaced `PWFSLSmoke::loadDataFile()` with `MazamaCoreUtils::loadDataFile()`.

# PWFSLSmoke 1.2.103

* docker version bump
* documentation typos

# PWFSLSmoke 1.2.102

* Ran `MazamaCoreUtils::lintFunctionArgs()` with `timezoneLintRules` and included
explicit timezones wherever they are allowed.
* `PWFSLSmoke::parseDatetime()` has been internally replaced with
  `MazamaCoreUtils::parseDatetime()` with explicit timezone argument

# PWFSLSmoke 1.2.101

* Fix bug in `monitor_load()` which wouldn't load data when the specified 
time range was in the last 10 days.

# PWFSLSmoke 1.2.100

* Version bump for CRAN release.

# PWFSLSmoke 1.2.11

This patch restores some of the ESRI mapping functionality by providing a 
function to download, stitch and crop images from ESRI tile services. The
new `staticmap_getRasterBrick()` provides a generalized API for generating
basemaps from the two tiling services currently being supported: ESRI and Stamen.

* Added `monitor_staticmap()`, `staticmap_getRasterBrick()` and 
`staticmap_getEsrimapBrick()`.
* Reordered arguments in `staticmap_getStamenmapBrick()`.

# PWFSLSmoke 1.2.10

 * Fixes for website documentation

# PWFSLSmoke 1.2.9

* Docker image now includes `optparse` package so that executable scripts can
  be run inside the `mazamascience/pwfslsmoke` image.
* Fixed error-generating bug in `debug.trace(...)` statement while processing
  AirNow data.

# PWFSLSmoke 1.2.8

This patch release deals with the AirNow's recent inclusion in their data feeds
of US Embassy data. Some Embassy sites are in time zones with fractional offset
from UTC. The PWFSLSmoke data model requires that all monitoring data align on
hour boundaries. All non-conforming AirNow monitors (mostly in South Asia) are
now removed during the early stages of processing.

# PWFSLSmoke 1.2.7

* Improved support for non-Olson timezones in `timeInfo()`.
* Corrected `timeInfo()` creation of `localSstandardTime_UTC`.

# PWFSLSmoke 1.2.6

* Updated `wrcc_createRawDataframe()` to use the proper WRCC url.

# PWFSLSmoke 1.2.5

* Updated the `WRCC` dataset with unitIDs added to the `cache` in 2018 and 2019.
* Updated WRCC processing functions to handle a slightly modified header format.

# PWFSLSmoke 1.2.4

This patch release deals with the loss of the free basemap generation service
utilized by `esriMap_getMap()`. An authentication token is now required and
this patch provides a workaround by using free tiles provided by
[Stamen](http://maps.stamen.com) and `ggmap::get_stamenmap()`.

The following new functions are provided:

* `monitor_stamenmap()` -- replacement for `monitor_esriMap()`
* `staticmap_getStamenmapBrick()`
* `staticmap_plotRasterBrick()`

The use of `monitor_esriMap()` has been purged from all examples and vignettes.
The goal is to get to the point of having at least minimal capabilities for
generating static maps and resubmitting the package to CRAN.

# PWFSLSmoke 1.2.3

 * removed `monitor_map()` examples because ESRI map downloads are disabled
 * added `createEmptyMonitor()`
 * fixes for `monitor_getCurrentStatus()`:
   - properly handling transitions between daylight savings and standard time
   - properly handling days with insufficient data to calculate an average

# PWFSLSmoke 1.2.2

 * CRAN submission fixes

# PWFSLSmoke 1.2.1

 * added an introductory vignette

# PWFSLSmoke 1.2.0

Version 1.2.x marks the official release for the 2019 fire season.

 * `monitor_leaflet()` now *visibly* returns a _leaflet_ object which can be
 further customized by the user.
 * code refactoring and cleanup unrelated to functionality --
 _.e.g._ indentation
 * logging changes in data processing functions:
   - internal logging statements use `TRACE` level
   (previously, many were `DEBUG`)
   - entering a function will generate a `DEBUG` level statement with the
   function name
 * bug fix for `monitor_getCurrentStatus()`

# PWFSLSmoke 1.1.29

## Breaking Changes

 * `monitor_getCurrentStatus()`
   - Argument `EndTime` now defaults to the most recent time in the given
     `ws_monitor`.
   - Ordering of columns in the output table have changed.

## Internals

 * `monitor_getCurrentStatus()`:
   - Code has been refactored to be more independent of prior steps.
   - Logging statements have been added.

# PWFSLSmoke 1.1.28

 * Fixed bug in parsing of arb2 monitors in "ebamMulti" format. (unitID 1048)

# PWFSLSmoke 1.1.27

 * Fixed bug in `monitor_leaflet()` which didn't properly assign colors when
 some monitors had all missing data.
 * `monitor_leaflet()` now displays monitors with all missing values as light
 gray.
 * `monitor_leaflet()` now invisibly returns a _leaflet_ object which can be
 further customized by the user.

# PWFSLSmoke 1.1.26

 * Now using the *geosphere* package to greatly improve the speed of distance
 calculations in `distance()`.
 * Fixed bug when calling `timeInfo(..., timezone = "UTC")`.
 * `monitor_dailyStatistic()` now uses the most common timezone when multiple
 timezones are encountered. (It previously just picked the first one.)
 * `monitor_getDailyMean()` now works with multiple-monitor _ws_monitor_
 objects and returns a dataframe of daily means

# PWFSLSmoke 1.1.25

Improved documentation and harmonization of `monitor_writeCSV()` and
`monitor_print()`.

# PWFSLSmoke 1.1.24

Minor cleanup in preparation for CRAN submission.

# PWFSLSmoke 1.1.23

This release adds functions for working with generic data.

## New Functions

 * `generic_downloadData()`
   - Reads data files as a string of data
 * `generic_parseData()`
   - Parses data based on a customizable configuration list

# PWFSLSmoke 1.1.22

 * fixed bug in `monitor_load()` where the incorrect year was used in requests
 for archival data

# PWFSLSmoke 1.1.21

 * lowered logging level inside `esriMap_getMap()` from `INFO` to `TRACE`
 * eliminated section from "Maps and Timeseries Plots" vignette that used
 now defunct `monitor_currentData()`

# PWFSLSmoke 1.1.20

## Breaking Change

 * `monitor_writeCurrentGeoJSON()` has been removed
 * `monitor_currentData()` has been removed

## New Functions

 * `monitor_writeCurrentStatusGeoJSON()`
   - This function is equivalent to `monitor_writeCurrentGeoJSON()`, but
     internally uses `monitor_getCurrentStatus()`

# PWFSLSmoke 1.1.19

This release enhances `monitor_getCurrentStatus()`.

 * Added 'summary' columns for previous times to match last times
 * Renamed `yesterdayAQI` column to `yesterday_pm25_24hr` for better fit with
   the column naming scheme
 * Added `last_validLocalTimestamp` and `previous_validLocalTimestamp` columns,
   which are strings showing the local time at a monitor

# PWFSLSmoke 1.1.18

This release refactors functions prefixed with `monitorPlot_*` to instead start
with `monitor_*`, keeping in line with other functions accepting a *ws_monitor*
object as their first argument.

The affected functions are:

| Old Name                      | New Name                    |
|-------------------------------|-----------------------------|
| `monitorPlot_dailyBarplot()`  | `monitor_dailyBarplot()`    |
| `monitorPlot_hourlyBarplot()` | `monitor_hourlyBarplot()`   |
| `monitorPlot_noData()`        | `monitor_noDataPlot()`      |
| `monitorPlot_rollingMean()`   | `monitor_rollingMeanPlot()` |
| `monitorPlot_timeseries()`    | `monitor_timeseriesPlot()`  |

In addition, `monitorPlot_timeOfDaySpaghetti()` has been marked as defunct,
with no replacement.

# PWFSLSmoke 1.1.17

  * added `tidy_toMonitor()`

Added new function `monitor_getCurrentStatus()`, which returns a data frame
containing meta information as well as current status information given a
*ws_monitor_* object.

# PWFSLSmoke 1.1.16

  * added additional logging to `EsriMap_getMap()`
  * added PWFSL monitoring site "mv4" AQI colors
  * `monitor_currentData()` bug fix

# PWFSLSmoke 1.1.15

 * spell check

# PWFSLSmoke 1.1.14

Add convenience functions `monitor_extractData()` and `monitor_extractMeta()`,
which return the dataframes inside a `ws_monitor` object.

# PWFSLSmoke 1.1.13

 * added for `pch` argument to `monitor_map()`

# PWFSLSmoke 1.1.12

This release refactors functions so that all functions accepting a `ws_monitor`
object as their first parameter include the `monitor_` prefix in their name.

The affected functions are:

| Old Name                   | New Name                   |
|----------------------------|----------------------------|
| `monitorDygraph()`         | `monitor_dygraph()`        |
| `monitorEsriMap()`         | `monitor_esriMap()`        |
| `monitorLeaflet()`         | `monitor_leaflet()`        |
| `monitorMap()`             | `monitor_map()`            |
| `monitorMap_performance()` | `monitor_performanceMap()` |

<br/>

In addition, `monitorGoogleMap()` has been deprecated in favor of
`monitor_esriMap()`.

# PWFSLSmoke 1.1.11

 * `montor_collapse()` now returns a `ws_monitor` object with a full 'meta'
 dataframe that retains any metadata that is shared
 * added `monitor_timeInfo()` convenience wrapper
 * added `localStandardTime_UTC` and `daylightSavings` columns to the dataframe
 returned by `timeInfo()`

# PWFSLSmoke 1.1.10

Moving all base plot related code from **PWFSLSmokePlot** to **PWFSLSmoke** so
that all functionality in the former is purely **ggplot2** based.

 * added `addPolygon()`, `addWindBarb() and `addWindBarbs()`
 * added `aqiPalette()`
 * added `monitor_getDailyMean()`


# PWFSLSmoke 1.1.9

 * `airnow_load()` calls `airnow_loadAnnual()` and will is *pre*-deprecated
 * `airsis_load()` calls `airsis_loadAnnual()` and will is *pre*-deprecated
 * `wrcc_load()` calls `wrcc_loadAnnual()` and will is *pre*-deprecated
 * renamed `loadData()` to `monitor_load()`
 * new dependency on *MazamaCoreUtils*
 * removed code associated with `DUMP` files
 * corrected spelling from `heidike` to `heidke` in all `skill` related functions

# PWFSLSmoke 1.1.8

 * `%>%` is now exported
 * added new `monitor_download~()` functions to download local copies of PWFSL
 monitor data files.
 * added new `monitor_loadLatest()`, `monitor_loadDaily()` and `monitor_loadAnnual()` functions
 * refactored `airnow`, `airsis`, `epa` and `wrcc` `~_load~()` functions to include
 a `dataDir` parameter to allow data loading from a local directory rather than
 always from the internet. the `airsis` and `wrcc` functions now support the
 (unused) `parameter` argument to match the `airnow` function signature

# PWFSLSmoke 1.1.7

 * added `loadData()` to load monitoring data covering any time period within a
 single year
 * fixed `monitor_join()` to skip over monitorIDs that are not found in either
 of the monitor objects being joined together

# PWFSLSmoke 1.1.6

 * actual parsing done by `lubridate::parse_date_time`
 * vectorized for inputs
 * handle any format of **Ymd[_HMS_]** (including multiple formats within same input)
 * new parameter `expectAll`, which controls how strict the parsing should be about accepting failures
 * added unit tests

# PWFSLSmoke 1.1.5

 * add `testthat` framework to package

# PWFSLSmoke 1.1.4

 * tweaks for CRAN submission
 * cleanup recommended by *goodpractice* package
 * removed non-functioning `monitorGooglMap()` and updated examples to use `monitorEsriMap()` instead
 * additional logging in `esriMap_getMap()`

# PWFSLSmoke 1.1.3

 * added logging functionality to `esriMap_getMap()`

# PWFSLSmoke 1.1.2

 * removed dependence on `zoo` package in favor of `tidyr` for filling data

# PWFSLSmoke 1.1.1

 * added linting configuration and suggest `lintr` package
 * added `PWFSLSmoke.Rproj` for consistent development
 * added Travis-CI configuration

# PWFSLSmoke 1.1.0

 * version bump

# PWFSLSmoke 1.0.33

 * updated docker/ to use mazamascience/spatialutils:0.5.4 which is based off of rocker/tidyverse:3.5.1
 * tweaks for CRAN submission

# PWFSLSmoke 1.0.32

 * new `monitor_toTidy()` function to convert `ws_monitor` objects into tidyverse 'tidy-formatted' data (see more [here](http://vita.had.co.nz/papers/tidy-data.html))
 * new `monitor_isTidy()` function to check if given data is in a 'tidy' format

# PWFSLSmoke 1.0.31

  * bug fix for "negative subscripts" error from `monitor_writeCurrentGeoJSON()`*
  * modified `Maps_and_Timeseries_Plots` vignette to use ESRI maps instead of Google maps.

# PWFSLSmoke 1.0.30

  * added support for parsing for AIRSIS ARB2 EBAM-Multi file format (ARB2 unitIDs 1044-1049)

# PWFSLSmoke 1.0.29

  * updated `monitor_nowcast()` algorithm to return `NA` when monitor data re missing. (See the NowCast vignette.)

# PWFSLSmoke 1.0.28

  * support for `datetime` parameter in `monitor_writeCurrentGeoJSON()` and `monitor_currentData()`

# PWFSLSmoke 1.0.27

  * added `monitor_writeCurrentGeoJSON()` function
  * `monitor_currentData()` returns metadata in addition to current data
  * added `monitor_writeCSV()` function
  * enabled `monitor_print()` `quietly=TRUE`

# PWFSLSmoke 1.0.26

  * added `monitor_currentData()` function

# PWFSLSmoke 1.0.25

 * added package environment with `googleApiKey`, `esriApiKey` and getter/setter functions for each
 * added `addEsriAddress()` function

# PWFSLSmoke 1.0.24

 * `airsis_createMonitorObject()` and `wrcc_createMonitorObject()` now both accept arguments `existingMeta` with
 a default value of `NULL` and `addGoogleMeta` and `addEsriMeta` with default values of `FALSE`
 * `airsis_createMetaDataframe()` and `wrcc_createMetaDataframe()` now both accept arguments `existingMeta` with
 a default value of `NULL` and `addGoogleMeta` and `addEsriMeta` with default values of `FALSE`
 * removed deprecated `addGoogleMetadata()`

# PWFSLSmoke 1.0.23

 * new `aqiColors()` function to simplify creation of color vectors for use in maps and plots
 * new `monitor_print()` function to generate human readable CSV files
 * fixed bug in `monitor_dailyStatistic()` which did not return the first daily average
 * fixed bug in `monitor_subsetByDistance()` which failed when no monitors were found within the radius
 * fixed bug in `monitor_subsetBy()` which failed when a filter used variables defined within a calling function

# PWFSLSmoke 1.0.22

 * fixed bug in `monitor_join()` which failed when a monitorID was missing from one of the *ws_monitor* objects being joined

# PWFSLSmoke 1.0.21

 * removed errant TAB character from AQI_es$names

# PWFSLSmoke 1.0.20

 * tweaks for CRAN submission

# PWFSLSmoke 1.0.19

 * fixed bug in `esriMap_getMap()` when `width != height`
 * fixed single-digit month parsing in `airnow_load()`
 * capitalized `AQI$names`: 'Good', 'Moderate', ...
 * added `addAQIStackedBars()` function
 * added example test for `monitor_load()` which then tests `monitor_combine()`
 * shrunk legend in `monitorDygraph()`
 * changed `monitorDygraph()` to default to UTC when more than one timezone is present
 * added `AQI$actions` -- text with suggested actions to protect health
 * added language specific versions of `AQI`: `AQI_en` and `AQI_es`
 * added `monitor_isMonitor()` to validate the structure of a *ws_monitor* object
 * updated example code in all functions
 * turning off grid lines with `~Lwd=0` in `monitorPlot_timeseries()` now works on Windows
 * `monitorPlot_dailyBarplot()` accepts `tlim` argument of class `POSIXct`

# PWFSLSmoke 1.0.18

 * fixed newly introduced bug in `monitor_combine()`

# PWFSLSmoke 1.0.17

 * new `loadLatest()` and `loadDaily`() functions

# PWFSLSmoke 1.0.16

 * tweaks for CRAN submission

# PWFSLSmoke 1.0.15

 * fixed chronological ordering bug in `monitor_combine()`
 * `wrcc_downloadData()` now uses `parseDatetime()` like other functions
 * `monitorID` argument is no longer required in `monitor_join()`. Defaults to joining all shared monitorIDs.

# PWFSLSmoke 1.0.14

 * corrected `baseUrl` argument in `wrcc_loadLatest()` and `wrcc_loadDaily()`

# PWFSLSmoke 1.0.13

 * fixed bug in `monitor_dailyStatistics()` that ignored the last full day. Thanks to [jmatchett](https://github.com/jmatchett-usgs) for the fix.

# PWFSLSmoke 1.0.12

 * changed `daily_avg` to `dailyAvg` in `monitor_dailyStatistics()` argument `extraColumns`

# PWFSLSmoke 1.0.11

 * new `monitor_asDataframe()` function
 * fixed bug in `monitor_dailyStatistics()` that appeared when the host computer runs in the UTC timezone

# PWFSLSmoke 1.0.10

 * tweaks for CRAN submission

# PWFSLSmoke 1.0.9

 * tweaks for CRAN submission
 * removed dependency on **xml2** and **rvest** packages

# PWFSLSmoke 1.0.8

 * removed dependency on **RCurl** package
 * internal `WRCC` object with unitIDs is now a list of lists
 * more consistent error handling and logging during WRCC data processing
 * updated `airnow_loadLatest()` function to access real-time, last 10 day files
 * new `airnow_loadDaily()` function to access daily-updated, last 45 day files
 * new `wrcc_load()` function to access pre-generated annual .RData files
 * updated `wrcc_loadLatest()` function to access real-time, last 10 day files
 * new `wrcc_loadDaily()` function to access daily-updated, last 45 day files

# PWFSLSmoke 1.0.7

 * more consistent error handling and logging during AIRSIS data processing
 * new `airsis_load()` function to access pre-generated annual .RData files

# PWFSLSmoke 1.0.6

 * consistent use of log levels during AirNow data processing
 * updated `airnow_load()` function to access pre-generated monthly .RData files
 * properly closing connections in `epa_load()` and `airnow_load()`

# PWFSLSmoke 1.0.5

 * tweak to `localExecutables/epa_createAnnualDataframes_exec.R`
 * removed `logger` statements from `monitor_combine()` and `monitor_join()`
 * fixed bug in `monitor_join()` when averaging overlapping measurements

# PWFSLSmoke 1.0.4

 * fixed vignette typos
 * new `epa_load()` function accesses pre-generated annual .RData files
 * fixed bug in `monitorPlot_timeseries()` when using `style="aqidots"`

# PWFSLSmoke 1.0.3

 * tweaks for CRAN submission
 * fixed bug in `monitor_subsetBy()` when filtering on metadata columns with `NAs` present

# PWFSLSmoke 1.0.2

 * `monitor~` functions now test for empty *ws_monitor* objects
 * new Github `localNotebooks/` directory contains detailed examples

# PWFSLSmoke 1.0.1

 * fixed bug in `monitor_dailyStatistic()` that didn't test for `minHours` properly

# PWFSLSmoke 1.0.0

 * changed the non-guaranteed columns in `ws_monitor$meta`
 * `ws_monitor$meta$monitorID` is now a combination of new metadata columns `siteID` and `instrumentID`
 * added `initializeMazamaSpatialUtils()` convenience function
 * new `US_52` vector of all US state codes including `DC` and `PR`
 * upgraded all `~_createMetaDataframes()` to produce v1.0 metadata
 * `airnow_createMetaDataframes()` filters incoming sites for `countryCode %in% c('CA','MX','US')`,
 discarding sites associated with other countries
 * `monitorPlot_timeseries()` argument `aqiDots` renamed to `aqidots`
 * new `airnow_createMonitorObjects()` function
 * new `addUSGSElevation()` function
 * deprecating `addGoogleMetadata()` in favor of new functions `addGoogleElevation()` and `addGoogleAddress()`
 * removed all `openaq_~` data download and processing functions
 * added `zeroMinimum` parameter to data ingest functions to specify whether negative values should be converted to zero
 * renamed `airnow_downloadData()` to `airnow_downloadParseData()`
 * new `monitor_join()` function allows you to merge the data of *ws_monitor* objects with shared monitorIDs
 * renamed `CarmelValley` dataset to `Carmel_Valley`
 * converted all uses of `GMT` to `UTC`
 * new `createEmptyMetaDataframe()` function
 * `monitor_subset~()` functions now return 'meta' dataframes with zero rows and 'data' dataframes with a single `datetime` column rather than `NULL` when no monitors exist in the subset.

# PWFSLSmoke 0.99.37

 * fixed bugs in `airsis_availableUnits()`
 * export of `AIRSIS` object containing a list of understood `unitTypes` (primarily for internal use)
 * new `examples/airsis_2017.R` demonstrating creation of an AIRSIS monitoring dataset for 2017.

# PWFSLSmoke 0.99.36

 * removed auto-execution of examples in `monitor_dailyStatistic()` and `monitor_dailyThreshold()` to avoid
 CRAN test timeouts

# PWFSLSmoke 0.99.35

 * removed extraneous file for CRAN upload

# PWFSLSmoke 0.99.34

 * handling for additional 'UnitID' column in AIRSIS output

# PWFSLSmoke 0.99.33

 * tweaked esriMap_~ examples for CRAN upload

# PWFSLSmoke 0.99.32

 * tweaked NowCast vignette for CRAN upload

# PWFSLSmoke 0.99.31

 * refactored `esriMap_getMap()` to use `httr` package
 * added NowCast vignette

# PWFSLSmoke 0.99.30

 * new `airsis_availableUnits()` function
 * new `AIRSIS` object with available monitor types
 * fix to `esriMap_getMap()` to ensure that projected maps are the correct size
 * `esriMap_getMap()` arguments changed to: `bboxString`

# PWFSLSmoke 0.99.29

 * functions for ESRI Maps added: `esriMap_getMap()`, `esriMap_plotOnStaticMap()`, `monitorEsriMap()`
 * new `addMarker()` function to add a marker to a plot

# PWFSLSmoke 0.99.28

 * `monitor_reorder()` now includes `dropMonitors=FALSE` default

# PWFSLSmoke 0.99.27

 * corrected `monitor_nowcast()` now calculates values after the first 2 valid measurements
 * new `monitor_aqi()` algorithm
 * changed EBAM and E-Sampler QC pm25 threshold to accept as valid any pm25 value up to 5000 ug/m3

# PWFSLSmoke 0.99.26

 * avoid using Google elevation service whenever monitor metadata is already present

# PWFSLSmoke 0.99.25

 * more detailed logging output

# PWFSLSmoke 0.99.24

 * avoid using Google address service whenever monitor metadata is already present

# PWFSLSmoke 0.99.23

 * bug fix for monitor assignment issues in WRCC and AIRSIS monitors that move

# PWFSLSmoke 0.99.22

 * correction to `monitor_nowcast()` algorithm
 * updated vignettes to use package datasets

# PWFSLSmoke 0.99.20

 * added support for parsing of BAM1020 data from AIRSIS

# PWFSLSmoke 0.99.19

 * bug fix in quality control processing to handle monitors with no PM2.5 measurements

# PWFSLSmoke 0.99.18

 * bug fix in parsing of E-Sampler data from AIRSIS

# PWFSLSmoke 0.99.17

 * bug fix in `monitorPlot_dailyBarplot()` time axis

# PWFSLSmoke 0.99.16

 * bug fix in creation of temporary monitors

# PWFSLSmoke 0.99.15

 * monitorID now uses `monitorName` and `deploymentID`

# PWFSLSmoke 0.99.14

 * improved use of `DEBUG` and `INFO` logging statements during AIRSIS and WRCC data processing
 * new `deploymentID` for temporary monitors based on `lon_lat` rather than cluster ID
 * reset to zero any negative pm25 values in that make it through QC (AIRSIS and WRCC only)
 * fixed duplicate hour flagging bug during QC
 * added `flagAndKeep` capability to `airsis_createRawDataframe()` and `wrcc_createRawDataframe()`
 * `raw_enhance()` now handles AIRSIS EBAM files

# PWFSLSmoke 0.99.13

 * refactored `airsisDump_createMonitorObject()` which was previously applying QC before splitting by monitorID
 * fixed WRCC E-Sampler multiplication bug in `raw_enhance()`

# PWFSLSmoke 0.99.12

 * fixed bug in `monitor_combine()` so that it now works when `monitorList` contains a single *ws_monitor* object
 * fixed bugs in `wrccDump_parseData()` and `airsisDump_parseData()` which failed when a monitor had a single record of data

# PWFSLSmoke 0.99.11

 * 'gnats' style timeseries plots now plotting squares to speed things up
 * maps now work when meta$stateCode is missing
 * `parseDatetime()` returns POSIXct unmodified
 * `monitor_subsetBy()` drops monitors when filter evaluates to NA
 * all parameters now called `longitude` and `latitude` instead of `lon` and `lat`
 * new `monitor_scaleData()` function
 * fixed `monitorMap_performance()` issue with legend colors

# PWFSLSmoke 0.99.10

 * added docker/ directory showing how to create a docker image
 * added app/ directory showing how to create a **jug** based web service

# PWFSLSmoke 0.99.9

 * added `epa_~()` data processing functions
 * added example datasets: Northwest_Megafires, CarmelValley
 * refactored many examples to use example datasets and run during `R CMD check`[
 * removed rowname creation for 'data' dataframe
 * renamed `addLegend()` -> `addAQILegend()`
 * added `addAQILines()`
 * lots of documentation fixes

# PWFSLSmoke 0.99.0

 * `monitorMap()` no longer creates legends
 * `monitorGoogleMap()` no longer creates legends
 * `monitorGoogleMap()` accepts `centerLon` and `centerLat` arguments
 * `monitorPlot_dailyBarplot()` accepts `labels_x_nudge` and `labels_y_nudge` arguments
 * `monitorPlot_hourlyBarplot()` accepts `labels_x_nudge` and `labels_y_nudge` arguments
 * fixed s`tyle="gnats"` in `monitorPlot_timeseries()`
 * new Mapping vignette

# PWFSLSmoke 0.9.7

 * new AIRSIS_Raw_Data vignette
 * new `airnow_loadLatest()` function
 * `monitor_collapse()` now has `na.rm=TRUE` argument

# PWFSLSmoke 0.9.6

 * renamed vignette to AIRSIS_Data_Handling and updated content
 * removed unused minCount parameter from `addClustering()`
 * improved documentation in `raw_enhance()`

# PWFSLSmoke 0.9.5

 * renamed `addShadedNights()` to addShadedNight()`
 * `addBullseye()` accepts more arguments and works with RgoogleMaps map objects
 * `monitorDygraph()` now accepts `tlim` argument instead of `dateWindow`
 * cleaned up various examples
 * removed localDNR/ and localData/ directories
 * removed unused test code from localExamples/

# PWFSLSmoke 0.9.4

 * new `addIcon()` function adds icons to maps and RgoogleMaps map objects

# PWFSLSmoke 0.9.3

 * First alpha release with functionality for downloading and processing PM2.5 monitoring
data from AirNow, AIRSIS and WRCC.

# PWFSLSmoke 0.8.6

 * improved `airsis_downloadHourlyData()` now uses `readr::read_delim() `

# PWFSLSmoke 0.8.5

# PWFSLSmoke 0.8.4

 * `monitor_combine()` now accepts a list of monitors instead of just two.
 * New `df_timeOfDaySpaghettiPlot()` for working with "df_" data (raw "engineering" data but cleaned up and augmented).
 * New `df_getHighlightDates()` function to find dates with unusual values in the "engineering" data.

# PWFSLSmoke 0.8.3

 * All new AIRSIS data processing

# PWFSLSmoke 0.8.1

 * Cleanup and regularization of plotting functions.

# PWFSLSmoke 0.8.0

 * Plotting functions added.

# PWFSLSmoke 0.7.2

 * Removed AQI breaks for 1-3 hr and 8hr. Now only using 24-hr, daily avg. breaks. The package now only supports 24-hour AQI breaks.
 * Modified function signatures for `monitor_timeseriesPlot()`, `monitor_map()` and `monitor_leaflet()` to remove/modify their use of the `AQIStyle` argument.
 * Updated localExamples/Washington_August_2015.R

# PWFSLSmoke 0.7.1

 * Added data model vignette.
 * Documentation improvements.
 * New example in [localExamples](https://github.com/MazamaScience/PWFSLSmoke/tree/master/localExamples) directory.

# PWFSLSmoke 0.7.0

 * Initial extraction/refactoring of base code from [wildfireSmoke](https://github.com/MazamaScience/wildfireSmoke) package.
