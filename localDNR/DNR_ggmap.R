###############################################################################
# DNR Pilot Burn Pojrect (2928)
#
# The R code for the DNR pilot burn project data analysis is broken down into 
# the following steps, each associated with a separate file:
#
# * DNR_downloadData.R   -- download, QC, reshape and convert into .RData format
# * DNR_ingestData.R     -- ingest previously converted data and peroform any cleanup
#                           (e.g. convert negative values of PM2.5 to 0.0)
#
# Once all of this work is done, we are ready for the plotting scripts:
#
# * DNR_timeseriesPlot.R -- timeseries plot for a monitor of interest
# * DNR_ggmap.R          -- map for a burn of interest
###############################################################################

# This DNR_ggmap.R script defines a function for creating maps for a named
# prescribed burn with markers for nearby fires and monitoring locations.

# Load and clean up all required data with:
#   source('localDNR/DNR_ingestData.R')


if ( FALSE ) {
  
  library(PWFSLSmoke)
  library(ggmap)
  
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
    finalMap <- DNR_ggmap(title, lon, lat, zoom, local_tlim, map_source, map_type)
    
    ggsave(filename, finalMap, device="png", width=6, height=6)
    
  }
  
  
}

# FUNCTION ------------

DNR_ggmap <- function(title="Title", lon=-121, lat=48, zoom=10,
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
  
  # Add daily and hourly max to metadata
  airsis$meta$maxHourly <- apply(airsis$data[,-1], 2, max, na.rm=TRUE)
  airsis$meta$maxDailyMean <- apply(airsis_daily$data[,-1], 2, max, na.rm=TRUE)
  airsis$meta$maxAQILevel <- factor(.bincode(airsis$meta$maxDailyMean, AQI$breaks_24))
  
  # Add daily and hourly max to metadata
  airnow$meta$maxHourly <- apply(airnow$data[,-1], 2, max, na.rm=TRUE)
  airnow$meta$maxDailyMean <- apply(airnow_daily$data[,-1], 2, max, na.rm=TRUE)
  airnow$meta$maxAQILevel <- factor(.bincode(airnow$meta$maxDailyMean, AQI$breaks_24))
  
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
  initialMap <- ggmap(initialMap) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  #coordBounds <- initialMap$data
  
  # CREATE FINAL MAP -----------------
  
  eventSize <- .bincode(zoom,c(0,8:18)) # TODO: Improve event pixel sizing
  ###janiceSize <- .bincode(janice_SMASubset$Accomplished.Tons.from.FS.fireportal,c(0,250,500,1000,1500,4000))+6
  col_airsisDaily <- AQI$colors[airsis$meta$maxAQILevel]
  airsis$meta$AQIColor <- col_airsisDaily
  col_airnowDaily <- AQI$colors[airnow$meta$maxAQILevel]
  col_blueskyEvents <- rep("red",nrow(bluesky_eventsSubset))
  col_janiceSMA <- rep("red", nrow(janice_SMASubset))
  shape_prescribedBurn <- ifelse(janice_SMASubset$DNR_Pilot.24.Hr.Advance,17,2)

  # Neeeded for legend
  maxLevels <- max(c(airsis$meta$maxAQILevel,airnow$meta$maxAQILevel), na.rm=TRUE)

  finalMap <- initialMap +
    
    ggtitle(title) +
    
    # AIRSIS monitors
    geom_point(data=airsis$meta, # AIRSIS
               aes(x=longitude, y=latitude, fill=maxAQILevel),
               size=8) +
    #          aes(x=longitude, y=latitude),
    #          size=8, color=col_airsisDaily) +
  
    # AIRSIS max hourly values
    geom_text(data=airsis$meta,
              aes(x=longitude, y=latitude),
              label=round(airsis_maxHourly),
              color="black") +
    
    # AirNow monitors
    geom_point(data=airnow$meta,
               aes(x=longitude, y=latitude, fill=maxAQILevel),
               size=8) +
               # aes(x=longitude, y=latitude),
               # size=8, color=col_airnowDaily) +
               
    # AirNow max hourly values
    geom_text(data=airnow$meta,
              aes(x=longitude, y=latitude),
              label=round(airnow_maxHourly),
              color="black") +
    
    # Bluesky events as red squares
    geom_point(data=bluesky_eventsSubset,
               aes(x=longitude, y=latitude),
               size=eventSize, shape=0, color="red", stroke=1) +
    
    # Rx burns from Janice's database, sized by tons consumed
    geom_point(data=janice_SMASubset,
               aes(x=Longitude, y=Latitude),
               size=6, shape=shape_prescribedBurn, col="red", stroke=2)
    
    # Legend
    ###theme(legend.title = element_text(colour="chocolate", size=16, face="bold"))
    # guides(color = guide_legend(override.aes = list(size = 5))) +
    # scale_fill_manual(values = c('lightskyblue1', 'lightpink'),
    #                   labels = c('HQ', 'LQ')) +
    # scale_colour_manual(name="Max Daily AQI",
    #                     values=AQI$colors[1:maxLevels],
    #                     labels=AQI$names[1:maxLevels])
  
    
    return(finalMap)
  
}
