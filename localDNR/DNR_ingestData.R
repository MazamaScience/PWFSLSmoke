###############################################################################
# DNR Pilot Burn Pojrect (2928)
#
# The R code for the DNR pilot burn project data analysis is broken down into 
# the following steps, each associated with a separate file:
#
# * DNR_downloadData.R   -- download, QC, reshape and convert into .RData format
# * DNR_ingestData.R     -- ingest previously converted data and peroform any cleanup
#                           (e.g. convert negative values of PM2.5 to 0.0)
#
# Once all of this work is done, we are ready for the plotting scripts:
#
# * DNR_timeseriesPlot.R -- timeseries plot for a monitor of interest
# * DNR_burnMap.R        -- map of all burns near a monitoring location
###############################################################################

# This DNR_ingestData.R script ingests and cleans up previously downloaded data.

library(PWFSLSmoke)

# Set up MazamaSpatialUtils for metadata creation
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up logging and log to the console all DEBUG and higher logging statements
logger.setup()
logger.setLevel(INFO)

start <- 20160901
end <- 20161015

# ----- Load data created by DNR_downloadData.R -------------------------------

load('localData/airnow_monitors.RData')
load('localData/airsis_rawList.RData')
load('localData/airsis_monitorList.RData')
load('localData/bluesky_eventsList.RData')

# ----- Assign negative values of PM2.5 to zero -------------------------------

# TODO:  assign negative PM2.5 to zero for airnow_monitors, airsis_rawList, airsis_monitorList


# ----- Load hand-edited Excel Spreadsheet from Janice Peterson ---------------

filepath=paste0(getwd(),'/localData/DNR\ SM\ eastside\ accomplishments\ fall\ 2016.xlsx')
janice_SMA <- readxl::read_excel(filepath, na="NA")

# remove rows with all missing
allMissingMask <- apply( janice_SMA, 1, function(x) { all(is.na(x)) } )
janice_SMA <- janice_SMA[!allMissingMask,]

# TODO:  assign lowerCamelCase names

# TODO:  convert so that 'datetime' has a POSIXct in 'GMT' while 'datestamp' has a "YYYY-MM-DD" character string


