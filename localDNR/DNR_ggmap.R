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

# Temporary variables for testing. Eventually will call in variables using function.
monitorName<-'Plain'
tlim <- c(20160916,20160920)
zoom <- 8

# FUNCTION ------------

DNR_ggmap <- function(monitorName="Plain",tlim=c(20160916,20160920),zoom=8) {

library(PWFSLSmoke)
library(ggmap)

# Create ws_monitor object for monitor of interest
monitorID <- monitorDict[[monitorName]]
if (monitorID %in% names(airsis_monitors$data)) {
  mon <- monitor_subset(airsis_monitors, monitorIDs=monitorDict[[monitorName]])
  source <- "airsis"
} else if ( monitorID %in% names(airnow_monitors$data) ) {
  mon <- monitor_subset(airnow_monitors, monitorIDs=monitorDict[[monitorName]])
  source <- "airnow"
} else {
  # stop warnings message
}
source

# Assign lat/lon for map center, based on monitor name passed in
lon <- mon$meta$longitude
lat <- mon$meta$latitude

# Use the following to bypass the subsetting, in case this step is causing problems...
# bluesky_eventsSubset <- bluesky_events
# janice_SMASubset <- janice_SMA
# airnow_monitorsSubset <- airnow_monitors
# airsis_monitorsSubset <- airsis_monitors

# SUBSET DATA BY TIME
# some mess to sort out with the date stuff here...including whether to include data from the end of interval.
# For now, including the data from the day that matches the end of the tlim.

tlim <- as.Date(as.character(tlim),format='%Y%m%d')
tlim[2] <- tlim[2]+1 #Adjust end date by a day so all data in range is included in plot

range(tlim)

# # subset the airnow and airsis ws_monitor objects by time (unlear if this step is needed)
# airnow_monitorsSubset <- monitor_subset(airnow_monitors,tlim=tlim)
# airsis_monitorsSubset <- monitor_subset(airsis_monitors,tlim=tlim)

# subset the monitor of interset by time
monSubset <- monitor_subset(mon,tlim=tlim)

# # Create combined ws_monitor object of airnow and airsis monitors for plotting (to develop further later)
mon2 <- monitor_combine(list(airsis_monitors,airnow_monitors))
mon2Subset <- monitor_subset(mon2,tlim=tlim)

# subset bluesky and janice data by time limits
bluesky_eventsSubset <- subset(bluesky_events,
                               bluesky_events$datetime>=tlim[1]&
                               bluesky_events$datetime<tlim[2])

janice_SMASubset <- subset(janice_SMA,
                           janice_SMA$datetime>=tlim[1]&
                             janice_SMA$datetime<tlim[2])

# CREATE RAW MAP RASTER -------------
# NOTE: There appears to be an issue with stamen maps, presumably because the URL actually returns
# NOTE: a png instead of a jpg, which breaks the get_map function... So, just use google for now.

initialMap <- get_map(location=c(lon,lat), source="google",maptype="terrain", zoom=zoom)
initialMap <- ggmap(initialMap)
#initialMap

#coordBounds <- initialMap$data

# SUBSET DATA BY MAP SCALE -------------

# for each of the subset lines below, may want to add some logic to warn or handle data a certain way
# if no data is found within the given bounds...

# SUBSET BY LOCATION -- MIGHT REMOVE THIS SECTION ----------
# subset_bluesky_events <- subset(bluesky_events,
#                                 bluesky_events$latitude<max(coordBounds$lat)&
#                                 bluesky_events$latitude>min(coordBounds$lat)&
#                                 bluesky_events$longitude<max(coordBounds$lon)&
#                                 bluesky_events$longitude>min(coordBounds$lon))
# 

# subset_janice_SMA <- subset(janice_SMA,
#                             janice_SMA$Latitude<max(coordBounds$lat)&
#                             janice_SMA$Latitude>min(coordBounds$lat)&
#                             janice_SMA$Longitude<max(coordBounds$lon)&
#                             janice_SMA$Longitude>min(coordBounds$lon))
# 
# subset_airnow_monitors <- monitor_subset(airnow_monitors,
#                                          xlim=c(min(coordBounds$lon),max(coordBounds$lon)),
#                                          ylim=c(min(coordBounds$lat),max(coordBounds$lat)))
# 
# subset_airsis_monitors <- monitor_subset(airsis_monitors,
#                                          xlim=c(min(coordBounds$lon),max(coordBounds$lon)),
#                                          ylim=c(min(coordBounds$lat),max(coordBounds$lat)))

# CALCULATE VALUES FOR MAP ---------

monSubset_dailyMax <- monitor_dailyStatistic(mon, max)
monSubset_maxDailyMax <- max(monSubset_dailyMax$data[,2])

monSubset_dailyMean <- monitor_dailyStatistic(mon, mean)
monSubset_maxDailyMax <- max(monSubset_dailyMax$data[,2])

#mon2Subset_dailyMax <- monitor_dailyStatistic(mon2Subset, max)
#maxDailiyMax <- max(dailyMax$data[,2])
#mon2Subset_hourlyMax <- apply(mon2Subset$data,2,max,na.rm=TRUE)



# CREATE FINAL MAP -----------------

janiceSize <- .bincode(janice_SMASubset$Accomplished.Tons.from.FS.fireportal,c(0,250,500,1000,1500,4000))+4

finalMap <- initialMap+
  ggtitle(paste("Fires Near",monitorName,"\n",tlim[1],"thru",tlim[2]-1))+
  geom_point(data=bluesky_eventsSubset, #bluesky hotspots
             aes(x=longitude, y=latitude),
             size=3, shape=2, color="red")+
  geom_point(data=janice_SMASubset, # Rx burns from Janice's database, sized by tons consumed
             aes(x=Longitude, y=Latitude,size=janiceSize),
             shape=2,color="orange")+
  geom_point(data=mon2Subset$meta, # Airnow and Airsis monitors
             aes(x=longitude, y=latitude),
             size=4, color="blue")+ # to adjust colors by AQI...
  geom_text(x=lon, y=lat, label=paste("  Max Daily Max:",monSubset_maxDailyMax),
            hjust=0,color="blue")
  
  
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
