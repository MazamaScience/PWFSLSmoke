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
# * DNR_burMap.R         -- map for a burn of interest
###############################################################################

# This DNR_burnMap.R script defines a function for creating maps for a named
# prescribed burn with markers for nearby fires and monitoring locations.

# Load and clean up all required data with:
#   source('localDNR/DNR_ingestData.R')


if ( FALSE ) {
  
  library(PWFSLSmoke)
  library(RgoogleMaps)
  
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
    maptype <- 'terrain'
    
    filename <- file.path(getwd(),paste0(unit,'.png'))
    basemap <- stringr::str_replace(filename,"\\.png","_base.png")
    png(filename=filename, width=640, height=640)
    DNR_burnMap(title, lon, lat, zoom, local_tlim, maptype, basemap)
    dev.off()
    
  }
  
  
}

# FUNCTION ------------

DNR_burnMap <- function(title="Title", lon=-121, lat=48, zoom=10,
                        local_tlim=c(20160916,20160920),
                        maptype='terrain',
                        basemap) {
  
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
  airsis$meta$maxAQILevel <- .bincode(airsis$meta$maxDailyMean, AQI$breaks_24)
  
  # Add daily and hourly max to metadata
  airnow$meta$maxHourly <- apply(airnow$data[,-1], 2, max, na.rm=TRUE)
  airnow$meta$maxDailyMean <- apply(airnow_daily$data[,-1], 2, max, na.rm=TRUE)
  airnow$meta$maxAQILevel <- .bincode(airnow$meta$maxDailyMean, AQI$breaks_24)
  
  # NOTE:  events data has "UTC" timestamps
  afterStart <- as.POSIXct(bluesky_events$datetime, tz="UTC") >= utc_start
  beforeEnd <- as.POSIXct(bluesky_events$datetime, tz="UTC") <= utc_end
  bluesky_eventsSubset <- bluesky_events[afterStart & beforeEnd, ]
  
  # NOTE:  janice_SMA has "UTC" timestamps
  afterStart <- janice_SMA$datetime >= utc_start
  beforeEnd <- janice_SMA$datetime <= utc_end
  janice_SMASubset <- janice_SMA[afterStart & beforeEnd, ]
  
  eventSize <- .bincode(zoom,c(0,8:18)) # TODO: Improve event pixel sizing
  cex_events <- 1.5 # for zoom level 10
  cex_janiceSMA <- 4
  ###janiceSize <- .bincode(janice_SMASubset$Accomplished.Tons.from.FS.fireportal,c(0,250,500,1000,1500,4000))+6
  col_airsisDaily <- AQI$colors[airsis$meta$maxAQILevel]
  airsis$meta$AQIColor <- col_airsisDaily
  col_airnowDaily <- AQI$colors[airnow$meta$maxAQILevel]
  col_blueskyEvents <- rep("red",nrow(bluesky_eventsSubset))
  col_janiceSMA <- rep("red", nrow(janice_SMASubset))
  pch_janiceSMA <- ifelse(janice_SMASubset$DNR_Pilot.24.Hr.Advance,2,2)
  lwd_janiceSMA <- ifelse(janice_SMASubset$DNR_Pilot.24.Hr.Advance,4,2)
  lwd_events <- 1.5
  
  # ----- Generate map ----------------------------------------------
  
  myMap <- GetMap(center=c(lat,lon), zoom=zoom, maptype=maptype,
                  destfile=basemap);
  
  # AIRSIS max daily mean
  PlotOnStaticMap(myMap, airsis$meta$latitude, airsis$meta$longitude,
                  add=FALSE, # First plot uses add=FALSE
                  mar=c(1,1,4,10),
                  cex=5, pch=16, col=col_airsisDaily)
  
  # AIRSIS max hourly values
  TextOnStaticMap(myMap, airsis$meta$latitude, airsis$meta$longitude,
                  add=TRUE,
                  labels=,round(airsis$meta$maxHourly),
                  col='black')
  
  # AirNow max daily mean
  PlotOnStaticMap(myMap, airnow$meta$latitude, airnow$meta$longitude,
                  add=TRUE,
                  cex=5, pch=16, col=col_airnowDaily)
  
  # AirNow max hourly values
  TextOnStaticMap(myMap, airnow$meta$latitude, airnow$meta$longitude,
                  add=TRUE,
                  labels=,round(airnow$meta$maxHourly),
                  col='black')
  
  # Bluesky events as red squares
  PlotOnStaticMap(myMap, bluesky_eventsSubset$latitude, bluesky_eventsSubset$longitude,
                  add=TRUE,
                  cex=cex_events, pch=0, col="red", lwd=lwd_events)
  
  # Rx burns from Janice's database
  PlotOnStaticMap(myMap, janice_SMASubset$Latitude, janice_SMASubset$Longitude,
                  add=TRUE,
                  cex=cex_janiceSMA, pch=pch_janiceSMA, col="red", lwd=lwd_janiceSMA)
  
  # ----- Annotations ---------------------------------------------------------
  
  title(title)
  
  legend(330, 320, bty='n', title="Max Daily AQI", xpd=NA,
         pch=16, pt.cex=2, col=AQI$colors[1:3],
         legend=AQI$names[1:3])
  
  legend(330, 200, bty='n', title="Max Hourly PM2.5", xpd=NA,
         pch='#',
         legend=c('over daily AQI'))
  
  legend(330, 120, bty='n', title="Fires", xpd=NA,
         pch=c(0,2,2), pt.cex=1.5, pt.lwd=c(1.5,2,4), col="red",
         legend=c('Satellite','Prescribed','Prescribed 24hr'))
  
}
