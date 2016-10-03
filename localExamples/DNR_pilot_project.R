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
Plain <- airsis_createMonitorObject('USFS', unitID='1033', startdate=20160901, enddate=20161031)
Plain_24 <- monitor_rollingMean(Plain, 24, align="right")

# Curlew -- USFS1034
Curlew <- airsis_createMonitorObject('USFS', unitID='1034', startdate=20160701, enddate=20161231)
Curlew_24 <- monitor_rollingMean(Curlew, 24, align="right")

# KettleFalls -- USFS1050
KettleFalls <- airsis_createMonitorObject('USFS', unitID='1050', startdate=20160701, enddate=20161231)
KettleFalls_24 <- monitor_rollingMean(KettleFalls, 24, align="right")

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


# -----------------------------------------------------------------------------
# BEGIN Work with "engineering" data

# This could be rolled up into a separate "airsis_createQCDataframe()" or some such.
###fileString <- airsis_downloadData('USFS', '1031', 20160701, 20161231, baseUrl="http://xxxx.airsis.com/vision/common/CSVExport.aspx?")
###fileString <- airsis_downloadData('USFS', '1033', 20160701, 20161231, baseUrl="http://xxxx.airsis.com/vision/common/CSVExport.aspx?")
###fileString <- airsis_downloadData('APCD', '1013', 20160701, 20161231, baseUrl="http://xxxx.airsis.com/vision/common/CSVExport.aspx?")
fileString <- airsis_downloadData('ARB2', '1023', 20160601, 20161231, baseUrl="http://xxxx.airsis.com/vision/common/CSVExport.aspx?")

df <- airsis_parseData(fileString)
df <- airsis_qualityControl(df)
df <- addClustering(df, lonVar='Longitude', latVar='Latitude', clusterDiameter=1000)

# Check number of deployments -- only 1
unique(df$deploymentID)

# Data varaibles in columns 7:16
plot(df[,7:16])

# Interesting ... For Plain (1033) it looks like high smoke is associated with
# * low wind
# * wind direction ~0
# * low temp
# * high humidity
# * low FT
plot(df[,c('ConcHr', 'W.S','W.D','AT','RHx','FT')])

# Let's add local time to examine hour-of-day
df$datetime <- lubridate::mdy_hms(df$Date.Time.GMT)
df$localtime <- lubridate::with_tz(df$datetime, 'America/Los_Angeles')
df$localHour <- lubridate::hour(df$localtime)

plot(df[,c('ConcHr', 'W.S','W.D','AT','RHx','FT', 'localHour')])

# Nice! From this we see that most hours, the wind is out of the N or NW
# but that sometimes it starts in the N and then backs through E and S before
# ending up NW in the evening

highlights <- raw_getHighlightDates(df, 'ConcHr', 'America/Los_Angeles', c(.10,Inf))

# Other by-hour-of-day plots
layout(matrix(seq(4),nrow=2))
raw_timeOfDaySpaghettiPlot(df, 'W.S', 'America/Los_angeles', highlightDates=highlights)
raw_timeOfDaySpaghettiPlot(df, 'ConcHr', 'America/Los_angeles', highlightDates=highlights)
raw_timeOfDaySpaghettiPlot(df, 'AT', 'America/Los_angeles', highlightDates=highlights)
raw_timeOfDaySpaghettiPlot(df, 'RHx', 'America/Los_angeles', highlightDates=highlights)
layout(1)






# END engineering data
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# BEGIN Work with all monitors in a single ws_monitor object

if (FALSE) {
  # Create a list of ws_monitor objects
  monitorList <- list()
  monitorList[[1]] <- airsis_createMonitorObject('USFS', '1012', 20160701, 20161231)
  monitorList[[2]] <- airsis_createMonitorObject('APCD', '1013', 20160701, 20161231)
  monitorList[[3]] <- airsis_createMonitorObject('USFS', '1031', 20160701, 20161231)
  monitorList[[4]] <- airsis_createMonitorObject('USFS', '1032', 20160701, 20161231)
  monitorList[[5]] <- airsis_createMonitorObject('USFS', '1033', 20160701, 20161231)
  monitorList[[6]] <- airsis_createMonitorObject('USFS', '1034', 20160701, 20161231)
  monitorList[[7]] <- airsis_createMonitorObject('USFS', '1050', 20160701, 20161231)
  
  # Combine into a single monitor object
  ws_monitor <- monitor_combine(monitorList)
  
  # Sitenames show that two deployments are associated with PWFSL testing in Seattle
  #   4           Seattle-Fremont Avenue North        Usk.WA..1032.__001
  #   7              Seattle-North 34th Street     Curlew.WA..1034.__001
  ws_monitor$meta[,c('siteName', 'monitorID')]
  
  # Remove the Seattle deployments
  monitorIDs <- ws_monitor$meta$monitorID
  badIDs <- c('Usk.WA..1032.__001', 'Curlew.WA..1034.__001')
  monitorIDs <- setdiff(monitorIDs, badIDs)
  ws_monitor <- monitor_subset(ws_monitor, monitorIDs=monitorIDs)
  
  # Find the maxima of the data columns (omitting 'datetime')
  sort(apply(ws_monitor$data[,-1], 2, max, na.rm=TRUE))
  
}

# END single ws_monitor_object
# -----------------------------------------------------------------------------
