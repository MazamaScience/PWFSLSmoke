## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)
# library(MazamaSpatialUtils); setSpatialDataDir('~/Data/Spatial'); loadSpatialData('NaturalEarthAdm1')

## ----setup---------------------------------------------------------------
library(PWFSLSmoke)
logger.setup()
logger.setLevel(ERROR)

## ------------------------------------------------------------------------
# plain <- airsis_createMonitorObject(20160901, 20161015, provider='USFS', unitID=1033 )

## ------------------------------------------------------------------------
#monitorPlot_rollingMean(plain)

## ----raw-----------------------------------------------------------------
df <- PWFSLSmoke::airsis_createRawDataframe(20160901, 20161015, provider='USFS', unitID=1033)

## ------------------------------------------------------------------------
df <- raw_enhance(df,rawSource='AIRSIS')

## ------------------------------------------------------------------------
rawPlot_windRose(df,ws.int=1)

