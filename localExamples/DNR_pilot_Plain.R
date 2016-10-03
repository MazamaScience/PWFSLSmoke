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
# TODO:  Get table of prescribed burns from WA DNR
# TODO:  Get RAWS met data from WRCC
# TODO:  Look at Bluesky model runs
# TODO:  Need a new "airsis_createRawDataframe.R" function so we can look at
# TODO:  wind speed and direction.
###############################################################################

library(PWFSLSmoke)

# Set up MazamaSpatialUtils for metadata creation
library(MazamaSpatialUtils)
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('NaturalEarthAdm1')

# Set up logging and log to the console all DEBUG and higher logging statements
logger.setup()
logger.setLevel(DEBUG)

# Plain (Fire Station) -- USFS1033
Plain <- airsis_createMonitorObject('USFS', unitID='1033', startdate=20160901, enddate=20161031)
Plain_24 <- monitor_rollingMean(Plain, 24, align="right")

# Prescribed burn
Natapoc_x <- c(lubridate::ymd_hms("2016-09-20 08:00:00", tz="America/Los_Angeles"),
              lubridate::ymd_hms("2016-09-21 08:00:00", tz="America/Los_Angeles"),
              lubridate::ymd_hms("2016-09-22 08:00:00", tz="America/Los_Angeles"))
Natapoc_y <- c(52, 52, 54)

# Other fires
Fishloop_x <- c(lubridate::ymd_hms("2016-09-08 08:00:00", tz="America/Los_Angeles"),
               lubridate::ymd_hms("2016-09-12 08:00:00", tz="America/Los_Angeles"),
               lubridate::ymd_hms("2016-09-26 08:00:00", tz="America/Los_Angeles"),
               lubridate::ymd_hms("2016-09-28 08:00:00", tz="America/Los_Angeles"))
Fishloop_y <- c(55, 80, 180, 90)


monitor_timeseriesPlot(Plain, shadedNight=TRUE, type='l', lwd=2, col='goldenrod')
monitor_timeseriesPlot(Plain_24, add=TRUE, type='l', lwd=4, col='salmon3')
abline(h=seq(50,300,50),lty='dotted')
title("PM2.5 at Plain, WA vs. Burning at Wenatchee River")

segments(Natapoc_x, 0, Natapoc_x, Natapoc_y, lwd=4, col='black')
segments(Fishloop_x, 0, Fishloop_x, Fishloop_y, lwd=4, col='dodgerblue')
