## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)
library(MazamaSpatialUtils); setSpatialDataDir('~/Data/Spatial'); loadSpatialData('NaturalEarthAdm1') 

## ----eval=FALSE----------------------------------------------------------
#  library(PWFSLSmoke)
#  logger.setup()
#  logger.setLevel(ERROR)

## ----include=FALSE-------------------------------------------------------
library(PWFSLSmoke)
logger.setup()
logger.setLevel(ERROR)

## ------------------------------------------------------------------------
plain <- airsis_createMonitorObject(unitID = 1033, startdate = 20160901,enddate = 20161015)

## ------------------------------------------------------------------------
monitorPlot_rollingMean(plain)

## ----raw-----------------------------------------------------------------
df <- PWFSLSmoke::airsis_createRawDataframe(startdate = 20160901,enddate=20161015, unitID = 1033)

## ------------------------------------------------------------------------
df <- raw_enhance(df,rawSource='AIRSIS')

## ------------------------------------------------------------------------
rawPlot_pollutionRose(df)

