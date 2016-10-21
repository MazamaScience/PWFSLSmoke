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
#   http://usfs.airsis.com/vision/UnitHistory.aspx?uid=1049 -- ESAM
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


monitorList <- get(load('DNR_monitorList.RData'))
dnr <- monitor_combine(monitorList)

names <- names(dnr$data)[-1]

for (name in names) {
  ws_monitor <- monitor_subset(dnr, monitorID=name)
  title <- ws_monitor$meta$siteName
  DNR_averagesPlot(ws_monitor, title)
}


# -----------------------------------------------------------------------------
# BEGIN Work with "engineering" data

df <- Plain_raw

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


# Prepare for openair
timeRange <- range(df$datetime)
timeAxis <- seq(timeRange[1], timeRange[2], by="hour")
oaDF <- data.frame(datetime=timeAxis)
oaDF <- dplyr::left_join(oaDF, df, by='datetime')
oaDF <- oaDF[,c('datetime','W.S','W.D','ConcHr')]
names(oaDF) <- c('date','ws','wd','pm25')
oaDF$pm25 <- oaDF$pm25*1000

oaDF <- openair::rollingMean(oaDF, pollutant='ws', width=3, new.name='ws_3')
oaDF <- openair::rollingMean(oaDF, pollutant='wd', width=3, new.name='wd_3')
oaDF <- openair::rollingMean(oaDF, pollutant='pm25', width=3, new.name='pm25_3')

openair::windRose(oaDF)

openair::pollutionRose(oaDF, 'pm25', angle=45)

# Create cardinal direction factors from 3-hour rolling mean wind
sector_wind <- oaDF$wd_3 + 22.5
sector_code <- .bincode(sector_wind, seq(22.5, 382.5, 45))
cardinalDirection <- factor(sector_code, labels=c('NE','E','SE','S','SW','W','NW','N'))
N <- cardinalDirection == "N"
NE <- cardinalDirection == "NE"
E <- cardinalDirection == "E"
SE <- cardinalDirection == "SE"
S <- cardinalDirection == "S"
SW <- cardinalDirection == "SW"
W <- cardinalDirection == "W"
NW <- cardinalDirection == "NW"

arrow_N <- '\u2191'
arrow_NE <- '\u2197'
arrow_E <- '\u2192'
arrow_SE <- '\u2198'
arrow_S <- '\u2193'
arrow_SW <- '\u2199'
arrow_SW <- '\u2190'
arrow_W <- '\u2196'

# Plot smoothed pm2.5
plot(oaDF$pm25_3 ~ oaDF$date, type='l')

atext(oaDF$date[N], 150, arrow_N, srt=0)
###text(oaDF$date[NE], 140, arrow_NE) # not found in font
text(oaDF$date[NE], 150, labels=arrow_N, srt=-45) # srt assumes ccw=positive
text(oaDF$date[NW], 150, labels=arrow_N, srt=45) # srt assumes ccw=positive
text(oaDF$date[SE], 130, labels=arrow_S, srt=45) # srt assumes ccw=positive
text(oaDF$date[SW], 130, labels=arrow_S, srt=-45) # srt assumes ccw=positive
text(oaDF$date[S], 130, arrow_S, srt=0)


# Different idea for background wind colors

# > levels(cardinalDirection)
# [1] "NE" "E"  "SE" "S"  "SW" "W"  "NW" "N" 
colors <- rep('transparent',8)
# Highlight NW
colors[1] <- adjustcolor('red',0.3) # NE
colors[2] <- adjustcolor('purple',0.1) # E
colors[3] <- adjustcolor('blue',0.2) # SE
colors[4] <- adjustcolor('blue',0.3) # S
colors[5] <- adjustcolor('blue',0.2) # SW
colors[6] <- adjustcolor('purple',0.1) # W
colors[7] <- adjustcolor('red',0.2) # NW
colors[8] <- adjustcolor('red',0.3) # N


plot(oaDF$date, oaDF$pm25_3, type='l', xlim=as.POSIXct(c(lubridate::ymd("2016-09-09"),lubridate::ymd("2016-09-18"))))
abline(v=oaDF$date, col=colors[cardinalDirection], lwd=5)

# Flusing Winds
windBin <- .bincode(df$W.S, c(-Inf,1,2,5,10,Inf))
for (i in 5) { colors[i] <- adjustcolor('blue',(i-1)/5) }
# source('DNR_utils.R')
# Create fancy woodburning index
stoveIndex <- woodStoveIndex(df, rollingWidth=12, rollingAlign='left')
stoveSmoke <- Plain$data[,2]
stoveSmoke[stoveIndex < 0.5] <- NA
#
monitor_timeseriesPlot(Plain, type='l', shadedNight=FALSE)
points(stoveSmoke ~ Plain$data$datetime, pch=16, col='purple')
abline(v=df$datetime, col=colors[windBin])
legend('topleft', legend=c("Woodstove Index", "0-1 m/s Wind", "1-2 m/s Wind", "2-5 m/s Wind"), fill=c('purple',colors[1],colors[2],colors[3]))

# END engineering data
# -----------------------------------------------------------------------------



