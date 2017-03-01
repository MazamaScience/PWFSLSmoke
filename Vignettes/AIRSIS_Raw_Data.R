## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----General Setup, message=FALSE----------------------------------------
library(PWFSLSmoke)
logger.setup()
logger.setLevel(ERROR)

## ----MazamaSpatialUtils--------------------------------------------------
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

## ----airsis_createRawDataframe-------------------------------------------
plain_AIRSIS_EBAM <- airsis_createRawDataframe(20160906,20161006,provider = 'USFS', unitID = 1033)
nile_AIRSIS_ESAM <- airsis_createRawDataframe(20160906,20161006,provider = 'USFS', unitID = 1049)

## ----All Attribute Sets--------------------------------------------------
sort(names(plain_AIRSIS_EBAM))
sort(names(nile_AIRSIS_ESAM))

## ----Shared Attribute Sets-----------------------------------------------
names(plain_AIRSIS_EBAM)[names(plain_AIRSIS_EBAM) %in% names(nile_AIRSIS_ESAM)]

## ----EBAM-Unique Attributes----------------------------------------------
names(plain_AIRSIS_EBAM)[!(names(plain_AIRSIS_EBAM) %in% names(nile_AIRSIS_ESAM))]

## ----E-Sampler-Unique Attributes-----------------------------------------
names(nile_AIRSIS_ESAM)[!(names(nile_AIRSIS_ESAM) %in% names(plain_AIRSIS_EBAM))]

## ----raw_enhance---------------------------------------------------------
plain_enhanced <- raw_enhance(plain_AIRSIS_EBAM,rawSource='AIRSIS')
nile_enhanced <- raw_enhance(nile_AIRSIS_ESAM,rawSource='AIRSIS')

## ----Shared Attribute Sets Enhanced--------------------------------------
names(plain_enhanced)[names(plain_enhanced) %in% names(nile_enhanced)]

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=5)

## ------------------------------------------------------------------------
plain <- airsis_createMonitorObject(20160914, 20160914, provider='USFS', unitID=1033)
monitorGoogleMap(plain, zoom=8)
monitorGoogleMap(plain, zoom=12, maptype='terrain')

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ------------------------------------------------------------------------
rawPlot_timeseries(plain_enhanced)

## ------------------------------------------------------------------------
rawPlot_timeseries(plain_enhanced, tlim=c(20160912,20160916), dayLwd=1)

## ------------------------------------------------------------------------
rawPlot_timeseries(plain_enhanced, tlim=c(20160912,20160916),
                   shadedNight=FALSE, shadedBackground="windSpeed",
                   sbLwd=4, dayLwd=1,
                   ylab="PM2.5 (ug/m3) and Temperature (deg C)",
                   main="PM2.5, Temperature, and Wind Speed")
rawPlot_timeseries(plain_enhanced, parameter='temperature', add=TRUE, lty=5, col='blue')

## ------------------------------------------------------------------------
rawPlot_timeOfDaySpaghetti(plain_enhanced,tlim = c(20160912,20160916),main="PM2.5 9/12/16-9/16/16",highlightDates = 20160913)

## ---- fig.height=3-------------------------------------------------------
oldpar <- par(mfrow=c(1,2),mar=c(2.5,1.5,1.5,1))
rawPlot_timeOfDaySpaghetti(plain_enhanced,parameter = 'temperature',tlim = c(20160912,20160916),main="Temperature",highlightDates = 20160913,xlab='')
rawPlot_timeOfDaySpaghetti(plain_enhanced,parameter = 'windSpeed',tlim = c(20160912,20160916),main="Wind Speed",highlightDates = 20160913,xlab='')
par(oldpar)

## ------------------------------------------------------------------------
rawPlot_windRose(plain_enhanced,tlim = c(20160912,20160916))

## ------------------------------------------------------------------------
rawPlot_pollutionRose(plain_enhanced,tlim = c(20160912,20160916))

## ------------------------------------------------------------------------
rawPlot_pollutionRose(plain_enhanced,tlim = c(20160912,20160916),normalize=TRUE)

## ------------------------------------------------------------------------
rawPlot_pollutionRose(plain_enhanced,tlim = c(20160913,20160913),normalize = TRUE)

## ------------------------------------------------------------------------
rawPlot_pollutionRose(plain_enhanced,tlim = c(20160929,20160929),normalize = TRUE)

