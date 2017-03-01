## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----airnow, message=FALSE-----------------------------------------------
library(PWFSLSmoke)
airnow <- airnow_load(startdate=20150531, enddate=20151101)
PacNW <- monitor_subset(airnow, stateCodes=c("WA", "OR", "ID"))
PacNW_24 <- monitor_rollingMean(PacNW, width=24)

## ----map-----------------------------------------------------------------
monitorMap(PacNW_24, slice=max)
addLegend(title="Max AQI", cex=0.7)

## ----leaflet-------------------------------------------------------------
monitorLeaflet(PacNW_24, slice=max)

## ----dygraph-------------------------------------------------------------
NezPerceIDs <- c("160571012","160690012","160690013","160690014","160490003","160491012")
NezPerce <- monitor_subset(PacNW, monitorIDs=NezPerceIDs)
monitorPlot_timeseries(NezPerce, style='gnats')

## ----august--------------------------------------------------------------
PacNW <- monitor_subset(PacNW, tlim=c(20150801,20150831), timezone="America/Los_Angeles")
PacNW_24 <- monitor_subset(PacNW_24, tlim=c(20150801,20150831), timezone="America/Los_Angeles")
NezPerce <- monitor_subset(NezPerce, tlim=c(20150801,20150831), timezone="America/Los_Angeles")

## ----dailyBarplot--------------------------------------------------------
layout(matrix(seq(6)))
par(mar=c(1,1,1,1))
for (monitorID in NezPerceIDs) {
  siteName <- NezPerce$meta[monitorID,'siteName']
  monitorPlot_dailyBarplot(NezPerce, monitorID=monitorID, main=siteName, axes=FALSE) 
}
par(mar=c(5,4,4,2)+.1)
layout(1)

## ----acute---------------------------------------------------------------
data <- PacNW$data[,-1] # omit 'datetime' column
maxPM25 <- apply(data, 2, max, na.rm=TRUE)
worstAcute <- names(sort(maxPM25, decreasing=TRUE))[1:6]
intersect(worstAcute, NezPerceIDs)
PacNW$meta[worstAcute[1],c('siteName','countyName','stateCode')]

## ------------------------------------------------------------------------
PacNW_dailyAvg <- monitor_dailyStatistic(PacNW, FUN=mean, minHours=20)
data <- PacNW_dailyAvg$data[,-1]
unhealthyDays <- apply(data, 2, function(x){ sum(x >= AQI$breaks_24[4], na.rm=TRUE) })
worstChronic <- names(sort(unhealthyDays, decreasing=TRUE))[1:6]
intersect(worstChronic, NezPerceIDs)
PacNW$meta[worstChronic[1],c('siteName','countyName','stateCode')]

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=5)

## ----googleMap-----------------------------------------------------------
fireLons <- c(-118.461,-117.679,-120.039,-119.002,-119.662)
fireLats <- c(48.756,46.11,47.814,48.338,48.519)
gmap <- monitorGoogleMap(PacNW_dailyAvg, zoom=7, slice=max) # bad arguments: centerLon=-118, centerLat=47
# addIcon('redFlame', fireLons, fireLats, map=gmap, expansion=0.2) # commented out because could not build
addLegend(cex=0.8)
title("August, 2015", line=-1.5, cex.main=2)

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----tribalMonitors_daily------------------------------------------------
Omak <- monitor_subset(PacNW, monitorIDs="530470013")
Kamiah <- monitor_subset(PacNW, monitorIDs="160490003")
layout(matrix(seq(2)))
monitorPlot_dailyBarplot(Omak, main="August 2015 Daiy AQI -- Omak, WA",
                         labels_x_nudge=0.8, labels_y_nudge=250)
monitorPlot_dailyBarplot(Kamiah, main="August Daily AQI -- Kamiah, ID",
                         labels_x_nudge=0.8, labels_y_nudge=250)
layout(1)

## ----tribalMonitors_diurnal----------------------------------------------
layout(matrix(seq(2)))
par(mar=c(3,4,4,2))
monitorPlot_timeOfDaySpaghetti(Omak, title="Aug 23-26 Diurnal Smoke -- Omak, WA",
                               xlab='', ylab='',
                               tlim=c(20150823,20150826))
monitorPlot_timeOfDaySpaghetti(Kamiah, title="Aug 23-26 Diurnal Smoke -- Kamiah, ID",
                               xlab='', ylab='',
                               tlim=c(20150823,20150826))
par(mar=c(5,4,4,2)+.1)
layout(1)

