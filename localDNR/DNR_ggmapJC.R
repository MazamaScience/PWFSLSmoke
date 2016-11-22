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
  
  unique_unit <- paste0(janice_SMA$Unit,'_',strftime(janice_SMA$Ignition.time,"%Y%m%d"))
  
  for ( row in 1:nrow(janice_SMA) ) {
    
    unit <- make.names(unique_unit[row])
    lon <- janice_SMA$Longitude[row]
    lat <- janice_SMA$Latitude[row]
    tons <- janice_SMA$Accomplished.Tons.from.FS.fireportal[row]
    ignitionTime <- janice_SMA$Ignition.time[row]
    ignitionTitleString <- strftime(ignitionTime,"%b %d + 4 days")
    tlim_start <- strftime(ignitionTime,"%Y%m%d")
    tlim_end <- strftime(ignitionTime + lubridate::ddays(5),"%Y%m%d")
    local_tlim <- as.numeric(c(tlim_start,tlim_end))
    title <- paste0(unit,' (',tons,' tons): ',ignitionTitleString)
    zoom <- 10
    map_source <- 'google'
    map_type <- 'terrain'
    
    filename <- file.path(getwd(),paste0(unit,'.png'))
    ###png(filename=unit, width=1280, height=1280)
    finalMap <- DNR_ggmapJC(title, lon, lat, zoom, local_tlim, map_source, map_type)
    ###print(finalMap)
    ###dev.off()
    
    ggsave(filename, finalMap, device="png", width=6, height=6)
    
  }
  

}

# FUNCTION ------------

DNR_ggmapJC <- function(title="Title", lon=-121, lat=48, zoom=10,
                        local_tlim=c(20160916,20160920),
                        map_source='google', map_type='terrain') {
  
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
  
  initialMap <- get_map(location=c(lon,lat), source=map_source, maptype=map_type, zoom=zoom)
  initialMap <- ggmap(initialMap)
  
  #coordBounds <- initialMap$data
  
  # CREATE FINAL MAP -----------------
  
  eventSize <- .bincode(zoom,c(0,8:18)) # TODO: Improve event pixel sizing
  ###janiceSize <- .bincode(janice_SMASubset$Accomplished.Tons.from.FS.fireportal,c(0,250,500,1000,1500,4000))+6
  col_airsisDaily <- AQI$colors[.bincode(airsis_maxDailyMean, AQI$breaks_24)]
  col_airnowDaily <- AQI$colors[.bincode(airnow_maxDailyMean, AQI$breaks_24)]
  shape_prescribedBurn <- ifelse(janice_SMASubset$DNR_Pilot.24.Hr.Advance,17,2)
  
  finalMap <- initialMap +
    
    ggtitle(title) +
    
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
               aes(x=Longitude, y=Latitude),
               size=6, shape=shape_prescribedBurn, color="red")
    
    
  
  #geom_text(data=mon2Subset_dailyMax$meta, #call out monitor names (to replace with max value during period)
  #          aes(x=longitude, y=latitude, label=paste("  Max:",mon2Subset_dailyMaxMax)),
  #          hjust=0,color="blue")+
  #geom_text(data=janice_SMASubset, # call out ignition times on Rx burns
  #          aes(x=Longitude, y=Latitude, label=paste("  Ignition:",Ignition.time)),
  #          hjust=0,color="orange")
  #  geom_text(aes(x=longitude,y=latitude,data=airsis_monitorsSubset$meta),label = paste("value goes here: ",max(4)), angle = 45, hjust = 0, color = "orange")+
  
  #scale_size_area(breaks=c(250,500,750,1000),labels = c(250,500,750,1000), name = "Legend Title...") #legend
  
  ###suppressWarnings( print(finalMap) )
  return(finalMap)
  

}
