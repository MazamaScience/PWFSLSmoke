## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----Setup, message=FALSE------------------------------------------------
library(PWFSLSmoke)
logger.setup()
logger.setLevel(ERROR)

## ----Spatial Utils-------------------------------------------------------
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

## ------------------------------------------------------------------------
ws_monitor <- airnow_load(20160901,20161015,stateCodes = "WA")
plain <- airsis_createMonitorObject(20160901, 20161015, provider='USFS', unitID=1033 )

## ------------------------------------------------------------------------
plain <- monitor_subset(plain,tlim = c(20160905,20160930))

## ------------------------------------------------------------------------
monitorPlot_rollingMean(plain)

## ----raw-----------------------------------------------------------------
df <- PWFSLSmoke::airsis_createRawDataframe(20160901, 20161015, provider='USFS', unitID=1033)

## ------------------------------------------------------------------------
df <- raw_enhance(df,rawSource='AIRSIS')

## ------------------------------------------------------------------------
rawPlot_windRose(df,ws.int=1)

