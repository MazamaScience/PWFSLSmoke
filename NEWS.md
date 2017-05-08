# Updates to the PWFSLSmoke R Package

```
Utilities for working with PM2.5 monitoring data available
from the US EPA, AirNow, AIRSIS, WRCC and others.
```

----

## Version 0.99 -- Regularized and Consistent (beta)

### PWFSLSmoke 0.99.17

 * bug fix in `monitorPlot_dailyBarplot()` time axis

### PWFSLSmoke 0.99.16

 * bug fix in creation of temporary monitors

### PWFSLSmoke 0.99.15

 * monitorID now uses `monitorName` and `deploymentID`

### PWFSLSmoke 0.99.14

 * improved use of `DEBUG` and `INFO` logging statments during AIRSIS and WRCC data processing
 * new `deploymentID` for temporary monitors based on `lon_lat` rather than cluster ID
 * reset to zereo any negative pm25 values in that make it through QC (AIRSIS and WRCC only)
 * fixed duplicate hour flagging bug during QC
 * added `flagAndKeep` capability to `airsis_createRawDataframe()` and `wrcc_createRawDataframe()`
 * `raw_enhance()` now handles AIRSIS EBAM files

### PWFSLSmoke 0.99.13

 * refactored `airsisDump_createMonitorObject()` which was previously applying QC before splitting by monitorID
 * fixed WRCC E-Sampler mutliplication bug in `raw_enhance()`

### PWFSLSmoke 0.99.12

 * fixed bug in `monitor_combine()` so that it now works when `monitorList` contains a single *ws_monitor* object
 * fixed bugs in `wrccDump_parseData()` and `airsisDump_parseData()` which failed when a monitor had a single record of data

### PWFSLSmoke 0.99.11

 * 'gnats' style timeseries plots now plotting squares to speed things up
 * maps now work when meta$stateCode is missing
 * `parseDatetime()` returns POSIXct unmodified
 * `monitor_subsetBy()` drops monitors when filter evaluates to NA
 * all parameters now called `longitude` and `latitude` instead of `lon` and `lat`
 * new `monitor_scaleData()` function
 * fixed `monitorMap_performance()` issue with legend colors
 
### PWFSLSmoke 0.99.10

 * added docker/ directory showing how to create a docker image
 * added app/ directory showing how to create a **jug** based web service

### PWFSLSmoke 0.99.9

 * added `epa_~()` data processing functions
 * added example datasets: Northwest_Megafires, CarmelValley
 * refactored many examples to use example datasets and run during `R CMD check`[
 * removed rowname creation for 'data' dataframe
 * renamed `addLegend()` -> `addAQILegend()`
 * added `addAQILines()`
 * lots of documentation fixes

### PWFSLSmoke 0.99.0

 * `monitorMap()` no longer creates legends
 * `monitorGoogleMap()` no longer creates legends
 * `monitorGoogleMap()` accepts `centerLon` and `centerLat` arguments
 * `monitorPlot_dailyBarplot()` accepts `labels_x_nudge` and `labels_y_nudge` arguments
 * `monitorPlot_hourlyBarplot()` accepts `labels_x_nudge` and `labels_y_nudge` arguments
 * fixed s`tyle="gnats"` in `monitorPlot_timeseries()`
 * new Mapping vignette


## Version 0.9 -- Full Functionality (alpha)

### PWFSLSmoke 0.9.7

 * new AIRSIS_Raw_Data vignette
 * new `airnow_loadLatest()` function
 * `monitor_collapse()` now has `na.rm=TRUE` argument

### PWFSLSmoke 0.9.6

 * renamed vignette to AIRSIS_Data_Handling and updated content
 * removed unused minCount parameter from `addClustering()`
 * improved documentation in `raw_enhance()`

### PWFSLSmoke 0.9.5

 * renamed `addShadedNights()` to addShadedNight()`
 * `addBullseye()` accepts more arguments and works with RgoogleMaps map objects
 * `monitorDygraph()` now accepts `tlim` argument instead of `dateWindow`
 * cleaned up varous examples
 * removed localDNR/ and localData/ directories
 * removed unused test code from localExamples/

### PWFSLSmoke 0.9.4

 * new addIcon() function addis icons to maps and RgoogleMaps map objects

### PWFSLSmoke 0.9.3

First alpha release with functionality for downloading and processing PM2.5 monitoring
data from AirNow, AIRSIS and WRCC.


## Version 0.8 -- Plotting Functions

### PWFSLSmoke 0.8.6

 * improved airsis_downloadHourlyData() now uses readr::read_delim() 

### PWFSLSmoke 0.8.5

### PWFSLSmoke 0.8.4

 * `monitor_combine()` now accepts a list of monitors instead of just two.
 * New `df_timeOfDaySpaghettiPlot()` for working with "df_" data (raw "engineering" data but cleaned up and augmented).
 * New `df_getHighlightDates()` function to find dates with unusual values in the "engineering" data.

### PWFSLSmoke 0.8.3

 * All new AIRSIS data processing

### PWFSLSmoke 0.8.1

 * Cleanup and regularization of plotting functions.

### PWFSLSmoke 0.8.0

 * Plotting functions added.


## Version 0.7 -- Initial Internal Release

### PWFSLSmoke 0.7.2

 * Removed AQI breaks for 1-3 hr and 8hr. Now only using 24-hr, daily avg. breaks. The package now only supports 24-hour AQI breaks.
 * Modified function signatures for `monitor_timeseriesPlot()`, `monitor_map()` and `monitor_leaflet()` to remove/modify their use of the `AQIStyle` argument.
 * Updated localExamples/Washington_August_2015.R

### PWFSLSmoke 0.7.1

 * Added data model vignette.
 * Documentation improvements.
 * New example in [localExamples](https://github.com/MazamaScience/PWFSLSmoke/tree/master/localExamples) directory.
 
### PWFSLSmoke 0.7.0

 * Initial extraction/refactoring of base code from [wildfireSmoke](https://github.com/MazamaScience/wildfireSmoke) package.
