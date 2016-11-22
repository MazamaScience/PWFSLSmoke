# This function should use the ggmap package to create a map with the following features:
#   DONE: arguments = (monitorName='Plain', tlim=c(20160916,20160920))
#   DONE: terrain map centered on a monitor
#   display monitor as a filled circle with a color indicating highest daily average during tlim
#   display monitor highest hourly value during tlim as text to the right of monitor circle
#   display prescribed burn locations from janice_SMA during tlim as open red triangles sized by Tons Consumed (FS)
#   DONE: display bluesky_events hotspots during tlim as small open orange(?) triangles

# THOUGHTS ----------

# Should we center on a specific fire, rather than on a specific monitor?
# Let's include monitor IDs

# LOAD DATA ------------
# Need to manually load data for now, per the following:
# setwd("~/Projects/PWFSLSmoke")
# source("/localDNR/DNR_ingestData.R") #DOESN'T WORK...?!?!?! Do this manually for now...

# TEMPORARY SETTINGS -------------

library(PWFSLSmoke)
library(ggmap)


# Temporary variables for testing. Eventually will call in variables using function.
if ( FALSE ) {
  
  name <- "Plain"
  lon <- airsis_monitors$meta[monitorDict[[name]],'longitude']
  lat <- airsis_monitors$meta[monitorDict[[name]],'latitude']
  zoom <- 10
  local_tlim <- c(20160912, 20160918)
  
}

# FUNCTION ------------

DNR_ggmapJC <- function(name="Plain", lon=-121, lat=48, zoom=10, local_tlim=c(20160916,20160920)) {
  
  # Get UTC version of local tlim
  tlim <- parseDatetime(local_tlim)
  utc_start <- tlim[1]
  utc_end <- tlim[2]
  lubridate::tz(tlim) <- "America/Los_Angeles"
  tlim <- strftime(tlim, "%Y%m%d%H", tz="UTC")
  
  # NOTE:  tlim[2] is the day after the last full day of data
  
  # NOTE:  monitor data has "UTC" timestamps
  airsis <- monitor_subset(airsis_monitors, tlim=tlim)
  airnow <- monitor_subset(airnow_monitors, tlim=tlim)
  
  airsis_daily <- monitor_dailyStatistic(airsis, mean, dayStart="midnight")
  airnow_daily <- monitor_dailyStatistic(airnow, mean, dayStart="midnight")
  
  airsis_maxDailyMean <- apply(airsis_daily$data[,-1], 2, max, na.rm=TRUE)
  airsis_maxHourly <- apply(airsis$data[,-1], 2, max, na.rm=TRUE)
  
  airnow_maxDailyMean <- apply(airnow_daily$data[,-1], 2, max, na.rm=TRUE)
  airnow_maxHourly <- apply(airnow$data[,-1], 2, max, na.rm=TRUE)
  
  # NOTE:  events data has "UTC" timestamps
  afterStart <- as.POSIXct(bluesky_events$datetime, tz="UTC") >= utc_start
  beforeEnd <- as.POSIXct(bluesky_events$datetime, tz="UTC") <= utc_end
  bluesky_eventsSubset <- bluesky_events[afterStart & beforeEnd, ]
  
  # NOTE:  janice_SMA has "UTC" timestamps
  afterStart <- janice_SMA$datetime >= utc_start
  beforeEnd <- janice_SMA$datetime <= utc_end
  janice_SMASubset <- janice_SMA[afterStart & beforeEnd, ]
  
  # CREATE RAW MAP RASTER -------------
  # NOTE: There appears to be an issue with stamen maps, presumably because the URL actually returns
  # NOTE: a png instead of a jpg, which breaks the get_map function... So, just use google for now.
  
  initialMap <- get_map(location=c(lon,lat), source="google",maptype="terrain", zoom=zoom)
  initialMap <- ggmap(initialMap)
  
  #coordBounds <- initialMap$data
  
  # CREATE FINAL MAP -----------------
  
  eventSize <- .bincode(zoom,c(0,8:18)) # TODO: Improve event pixel sizing
  janiceSize <- .bincode(janice_SMASubset$Accomplished.Tons.from.FS.fireportal,c(0,250,500,1000,1500,4000))+6
  col_airsisDaily <- AQI$colors[.bincode(airsis_maxDailyMean, AQI$breaks_24)]
  col_airnowDaily <- AQI$colors[.bincode(airnow_maxDailyMean, AQI$breaks_24)]
  
  finalMap <- initialMap +
    
    ggtitle(paste("Fires Near",name,"\n",local_tlim[1],"thru",local_tlim[2]-1)) +
    
    geom_point(data=airsis$meta, # AIRSIS
               aes(x=longitude, y=latitude),
               size=5, color=col_airsisDaily) +
  
    geom_text(data=airsis$meta, # AIRSIS
              aes(x=longitude, y=latitude),
              label=paste0("   max ", round(airsis_maxHourly)),
              hjust=0, color="black") +
    
    geom_point(data=airnow$meta, # AirNow
               aes(x=longitude, y=latitude),
               size=5, color=col_airnowDaily) +
    
    geom_text(data=airnow$meta, # AirNow
              aes(x=longitude, y=latitude),
              label=paste0("   max ", round(airnow_maxHourly)),
              hjust=0, color="black") +
  
    geom_point(data=bluesky_eventsSubset, # bluesky hotspots as red squares
               aes(x=longitude, y=latitude),
               size=eventSize, shape=0, color="red") +
    
    geom_point(data=janice_SMASubset, # Rx burns from Janice's database, sized by tons consumed
               aes(x=Longitude, y=Latitude, size=janiceSize),
               shape=17, color="orange")
    
    
  
  #geom_text(data=mon2Subset_dailyMax$meta, #call out monitor names (to replace with max value during period)
  #          aes(x=longitude, y=latitude, label=paste("  Max:",mon2Subset_dailyMaxMax)),
  #          hjust=0,color="blue")+
  #geom_text(data=janice_SMASubset, # call out ignition times on Rx burns
  #          aes(x=Longitude, y=Latitude, label=paste("  Ignition:",Ignition.time)),
  #          hjust=0,color="orange")
  #  geom_text(aes(x=longitude,y=latitude,data=airsis_monitorsSubset$meta),label = paste("value goes here: ",max(4)), angle = 45, hjust = 0, color = "orange")+
  
  #scale_size_area(breaks=c(250,500,750,1000),labels = c(250,500,750,1000), name = "Legend Title...") #legend
  
  finalMap
  
  # suppressWarnings(expr)
  
}
