library(PWFSLSmoke)
library(PWFSLSmokeModeling)
library(MazamaSpatialUtils); setSpatialDataDir('~/Data/Spatial'); loadSpatialData('NaturalEarthAdm1')
library("animation", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
setModelDataDir('~/Data/Bluesky')
logger.setup()

airnow_ozone <- airnow_loadLatest(file = 'AirNowTech_OZONE_LATEST.RData')

AQI_ozone <- AQI # initialize with the PM2.5 AQI (colors and names are the same between the two)
# set the ozone AQI breakpoints
AQI_ozone$breaks_24[2]=55  # beg yellow (moderate)
AQI_ozone$breaks_24[3]=71  # beg orange (USG)
AQI_ozone$breaks_24[4]=86  # beg red (unhealthy)
AQI_ozone$breaks_24[5]=106 # beg purple (very unhealthy)
AQI_ozone$breaks_24[6]=201 # beg brown (hazardous)

mmdd = c("0826","0827","0828","0829","0830","0831","0901","0902","0903","0904","0905","0906","0907","0908","0909","0910","0911","0912","0913","0914","0915","0916","0917","0918","0919","0920","0921","0922","0923","0924","0925","0926","0927","0928","0929","0930","1001","1002","1003","1004","1005")
hh_beg = c("09","08","07","06")
hh_end = c("08","07","06","05")
mytz = c("America/Los_Angeles","America/Denver","America/Chicago","America/New_York")

#i<-10
#j<-2
begDate = paste("2017",mmdd[10],hh_beg[2], sep = "") # 1am standard time
endDate = paste("2017",mmdd[11],hh_end[2], sep = "") # midnight standard time
airnow_ozone_1day <- monitor_subset(airnow_ozone,tlim = c(begDate,endDate)) #get one day of hrly data

# calculate the rolling 8 hr mean for all the monitors
airnow_ozone_1day_8hr <- monitor_rollingMean(airnow_ozone_1day,width=8,align="left")

# map the values. By default monitorLeaflet chooses the max value at each monitor.
# Note that this gives wrong map
# In particular, see the 4 Wyoming monitors as an example.
monitorLeaflet(airnow_ozone_1day_8hr, breaks = AQI_ozone$breaks_24, colors = AQI_ozone$colors)
# if I look at the data for Casper, the max is 107, which should map as purple. Instead, it is mapping as green
airnow_ozone_1day_8hr$data$'560250005'
#[1] 107.125000  96.625000  86.500000  74.000000  61.250000  48.375000  35.500000  24.000000  17.750000  13.875000   9.875000   8.250000   6.500000   4.875000   3.625000   3.125000   2.750000
#[18]   2.571429         NA         NA         NA         NA         NA         NA

# now subset by tz the same data that was just mapped incorrectly. Now it maps correctly.
# Look at the 4 Wyoming monitors in particular
airnow_ozone_tz <- monitor_subsetBy(airnow_ozone_1day_8hr,timezone==mytz[2])
monitorLeaflet(airnow_ozone_tz, breaks = AQI_ozone$breaks_24, colors = AQI_ozone$colors)
