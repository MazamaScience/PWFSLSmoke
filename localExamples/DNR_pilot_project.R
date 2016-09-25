###############################################################################
# Obtain and plot monitoring data for the DNR pilot project
#
# Susan O'Neill provided these data URLs:
#
#   http://www.wrcc.dri.edu/cgi-bin/rawMAIN4.pl?idsmf1 
#   http://apcd.airsis.com/vision/UnitHistory.aspx?uid=1013
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1033
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1031
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1034
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1012 -- EBAM
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1050 -- ESAM
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1032
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

# Plain (Fire Station) -- USFS1033
Plain <- airsis_createMonitorObject('USFS', unitID='1033', startdate=20160701, enddate=20161231)
Plain_24 <- monitor_rollingMean(Plain, 24, align="right")

monitor_timeseriesPlot(Plain, AQIStyle="24", shadedNight=TRUE)
col_24 <- adjustcolor('purple', 0.5)
monitor_timeseriesPlot(Plain_24, type='l', col=col_24, lwd=4, add=TRUE)
title("Hourly PM2.5 -- Plain, Washington")
legend('topleft', "24-hour average", col=col_24, lwd=4)

# KettleFalls -- USFS1050
KettleFalls <- airsis_createMonitorObject('USFS', unitID='1050', startdate=20160701, enddate=20161231)
KettleFalls_24 <- monitor_rollingMean(KettleFalls, 24, align="right")

monitor_timeseriesPlot(KettleFalls, AQIStyle="24", shadedNight=TRUE)
col_24 <- adjustcolor('purple', 0.5)
monitor_timeseriesPlot(KettleFalls_24, type='l', col=col_24, lwd=4, add=TRUE)
title("Hourly PM2.5 -- Kettle Falls, Washington")
legend('topleft', "24-hour average", col=col_24, lwd=4)


