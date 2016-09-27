###############################################################################
# Obtain and plot monitoring data for the DNR pilot project
#
# Susan O'Neill provided these data URLs:
#
#   http://www.wrcc.dri.edu/cgi-bin/rawMAIN4.pl?idsmf1 
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1012 -- EBAM
#   http://apcd.airsis.com/vision/UnitHistory.aspx?uid=1013 -- EBAM
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1031 -- EBAM
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1032 -- EBAM
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1033 -- EBAM
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1034 -- EBAM
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1050 -- ESAM
#
###############################################################################

library(PWFSLSmoke)

# Set up MazamaSpatialUtils for metadata creation
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up logging and log to the console all DEBUG and higher logging statements
logger.setup()
logger.setLevel(INFO)

# Fish Hatchery -- USFS1012
FishHatchery <- airsis_createMonitorObject('USFS', unitID='1012', startdate=20160701, enddate=20161231)
FishHatchery_24 <- monitor_rollingMean(FishHatchery, 24, align="right")

# KennedyMeadows -- USFS1013
KennedyMeadows <- airsis_createMonitorObject('APCD', unitID='1013', startdate=20160701, enddate=20161231)
KennedyMeadows_24 <- monitor_rollingMean(KennedyMeadows, 24, align="right")

# Liberty -- USFS1031
Liberty <- airsis_createMonitorObject('USFS', unitID='1031', startdate=20160701, enddate=20161231)
Liberty_24 <- monitor_rollingMean(Liberty, 24, align="right")

# Usk -- USFS1032
Usk <- airsis_createMonitorObject('USFS', unitID='1032', startdate=20160701, enddate=20161231)
Usk_24 <- monitor_rollingMean(Usk, 24, align="right")

# Plain (Fire Station) -- USFS1033
Plain <- airsis_createMonitorObject('USFS', unitID='1033', startdate=20160701, enddate=20161231)
Plain_24 <- monitor_rollingMean(Plain, 24, align="right")

# Curlew -- USFS1034
Curlew <- airsis_createMonitorObject('USFS', unitID='1034', startdate=20160701, enddate=20161231)
Curlew_24 <- monitor_rollingMean(Curlew, 24, align="right")

# KettleFalls -- USFS1050
KettleFalls <- airsis_createMonitorObject('USFS', unitID='1050', startdate=20160701, enddate=20161231)
KettleFalls_24 <- monitor_rollingMean(KettleFalls, 24, align="right")

# -- OR --

monitorList <- list()
monitorList[[1]] <- airsis_createMonitorObject('USFS', '1012', 20160701, 20161231)
monitorList[[2]] <- airsis_createMonitorObject('APCD', '1013', 20160701, 20161231)
monitorList[[3]] <- airsis_createMonitorObject('USFS', '1031', 20160701, 20161231)
monitorList[[4]] <- airsis_createMonitorObject('USFS', '1032', 20160701, 20161231)
monitorList[[5]] <- airsis_createMonitorObject('USFS', '1033', 20160701, 20161231)
monitorList[[6]] <- airsis_createMonitorObject('USFS', '1034', 20160701, 20161231)
monitorList[[7]] <- airsis_createMonitorObject('USFS', '1050', 20160701, 20161231)
ws_monitor <- monitor_combine(monitorList)



monitor_timeseriesPlot(Plain, AQIStyle="24", shadedNight=TRUE)
col_24 <- adjustcolor('purple', 0.5)
monitor_timeseriesPlot(Plain_24, type='l', col=col_24, lwd=4, add=TRUE)
title("Hourly PM2.5 -- Plain, Washington")
legend('topleft', "24-hour average", col=col_24, lwd=4)

monitor_timeseriesPlot(KettleFalls, AQIStyle="24", shadedNight=TRUE)
col_24 <- adjustcolor('purple', 0.5)
monitor_timeseriesPlot(KettleFalls_24, type='l', col=col_24, lwd=4, add=TRUE)
title("Hourly PM2.5 -- Kettle Falls, Washington")
legend('topleft', "24-hour average", col=col_24, lwd=4)


