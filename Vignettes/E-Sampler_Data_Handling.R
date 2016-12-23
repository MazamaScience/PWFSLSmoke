## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----setup---------------------------------------------------------------
library(PWFSLSmoke)
logger.setup()
logger.setLevel(ERROR)

## ----download------------------------------------------------------------
fileStringEBAM <- airsis_downloadData('USFS', unitID='1012',
                                      startdate=20160901,
                                      enddate=20160930)
fileStringESAM <- airsis_downloadData('USFS', unitID='1050',
                                      startdate=20160901,
                                      enddate=20160930)

## ----parse---------------------------------------------------------------
dfEBAM <- airsis_parseData(fileStringEBAM)
dfESAM <- airsis_parseData(fileStringESAM)

## ----timingPlot----------------------------------------------------------
tsESAM <- lubridate::mdy_hms(dfESAM$TimeStamp)
minsESAM <- lubridate::minute(tsESAM)
plot(tsESAM, minsESAM, ylim=c(0,25), xlab="TimeStamp", ylab="TimeStamp Minute")
title("Minutes Past the Top of the Hour\nUSFS Monitor ID 1050 TimeStamp Data\nSeptember 2016")

