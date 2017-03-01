## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----setup---------------------------------------------------------------
library(PWFSLSmoke)
logger.setup()
logger.setLevel(INFO)

## ----spatial_utils-------------------------------------------------------
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

## ----airsis_createMonitorObject------------------------------------------
usfs_1013 <- airsis_createMonitorObject(20150301, 20150831, 'USFS', unitID='1013')

## ----deploymentID--------------------------------------------------------
usfs_1013$meta$monitorID

## ----download------------------------------------------------------------
logger.setLevel(ERROR)
fileStringEBAM <- airsis_downloadData(startdate=20160901,
                                      enddate=20160930,
                                      provider='USFS',
                                      unitID='1012')
fileStringESAM <- airsis_downloadData(startdate=20160901,
                                      enddate=20160930,
                                      provider='USFS',
                                      unitID='1050')

## ----parse---------------------------------------------------------------
dfEBAM <- airsis_parseData(fileStringEBAM)
dfESAM <- airsis_parseData(fileStringESAM)

## ----timingPlot----------------------------------------------------------
tsESAM <- lubridate::mdy_hms(dfESAM$TimeStamp)
minsESAM <- lubridate::minute(tsESAM)
plot(tsESAM, minsESAM, ylim=c(0,25), xlab="TimeStamp", ylab="TimeStamp Minute")
title("Minutes Past the Top of the Hour\nUSFS Monitor ID 1050 TimeStamp Data\nSeptember 2016")

## ----raw-----------------------------------------------------------------
usfs_1013_raw <- airsis_createRawDataframe(20150301, 20150831, 'USFS', unitID='1013')
usfs_1013_enhanced <- raw_enhance(usfs_1013_raw)

## ----pollutionRose-------------------------------------------------------
rawPlot_pollutionRose(usfs_1013_enhanced)

