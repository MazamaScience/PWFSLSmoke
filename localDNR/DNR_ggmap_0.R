# This function should use the ggmap package to create a map with the following features:
#   DONE: arguments = (monitorName='Plain', tlim=c(20160916,20160920))
#   DONE: terrain map centered on a monitor
#   DONE: display monitor as a filled circle with a color indicating highest daily average during tlim
#   DONE: display monitor highest hourly value during tlim as text to the right of monitor circle
#   DONE: display prescribed burn locations from janice_SMA during tlim as open red triangles sized by Tons Consumed (FS)
#   DONE: display bluesky_events hotspots during tlim as small open orange(?) triangles

# LOAD DATA ------------

DNR_ggmap <- function(monitorName="Plain",tlim=c(20160916,20160920),zoom=8) {

  # Temporary settings
  #monitorName<-'Plain'
  #tlim <- c(20160901,20160920)
  #zoom <- 10

library(PWFSLSmoke)
library(ggmap)
source('~/Projects/PWFSLSmoke/localDNR/DNR_ingestData.R', echo=FALSE)

# ----------

# Create ws_monitor object for monitor of interest
monitorID <- monitorDict[[monitorName]]
if (monitorID %in% names(airsis_monitors$data)) {
  mon <- monitor_subset(airsis_monitors, monitorIDs=monitorDict[[monitorName]])
  #source <- "airsis"
} else if ( monitorID %in% names(airnow_monitors$data) ) {
  mon <- monitor_subset(airnow_monitors, monitorIDs=monitorDict[[monitorName]])
  #source <- "airnow"
} else {
  # stop warnings message
}

# SUBSET DATA BY TIME --------------
# some mess to sort out with the date stuff here...including whether to include data from the end of interval.
# For now, including the data from the day that matches the end of the tlim.
# May also want to consider including janice data from a day or two prior to given interval (e.g. for lingering, smoldering fires)

tlimD <- as.Date(as.character(tlim),format='%Y%m%d')
tlimD[2] <- tlimD[2]+1 #Adjust end date by a day so all data in range is included in plot

mon <- monitor_subset(mon,tlim=tlimD)
bluesky_events <- subset(bluesky_events,
                         bluesky_events$datetime>=tlimD[1]&
                           bluesky_events$datetime<tlimD[2])
janice_SMA <- subset(janice_SMA,
                     janice_SMA$datetime>=tlimD[1]&
                       janice_SMA$datetime<tlimD[2])

# CREATE RAW MAP RASTER -------------
# NOTE: There appears to be an issue with stamen maps, presumably because the URL actually returns
# NOTE: a png instead of a jpg, which breaks the get_map function... So, just use google for now.

# Assign lat/lon for map center, based on monitor name passed in
lon <- mon$meta$longitude
lat <- mon$meta$latitude

initialMap <- get_map(location=c(lon,lat), source="google",maptype="terrain", zoom=zoom)
initialMap <- ggmap(initialMap)
initialMap <- initialMap+
  theme(axis.title.x=element_blank(),
        #panel.border=element_blank(),
        axis.title.y=element_blank())
  

# CALCULATE VALUES FOR MAP ---------

mon_dailyMean <- monitor_dailyStatistic(mon, mean)
mon_maxDailyMean <- max(mon_dailyMean$data[,2]) #max daily mean (for colors)

mon_maxHourlyValue <- max(mon$data[,2],na.rm=TRUE) #max hourly value (for label)

# Color of max daily mean
mon_maxDailyMeanColor <- AQI$colors[.bincode(mon_maxDailyMean,AQI$breaks_24,include.lowest = TRUE)]

# CREATE FINAL MAP -----------------

sizeBins <- c(0,250,500,2500,5000)
janice_SMA$plotSize <- .bincode(janice_SMA$Accomplished.Tons.from.FS.fireportal,sizeBins)+3

finalMap <- initialMap+
  ggtitle(paste("Fires Near",monitorName,"\n",tlimD[1],"through",tlimD[2]-1))+
  geom_point(data=mon$meta, # Monitor of interest, color coded by max daily mean AQI
             aes(x=longitude, y=latitude),
             color=mon_maxDailyMeanColor, size=5)+
  #scale_color_discrete(name = "AQI")+#,breaks=c(1,2,3),labels=c(1,2,3))+
  geom_point(data=janice_SMA, # Rx burns from Janice's database, sized by tons consumed
             aes(x=Longitude, y=Latitude,size=plotSize),
             shape=2,stroke=2,color="red")+
  geom_point(data=bluesky_events, #bluesky hotspots
             aes(x=longitude, y=latitude),
             size=4, shape=2, color="orange")+
  geom_text(x=lon, y=lat, label=paste("   Max Hourly Value:",mon_maxHourlyValue),
            hjust=0,color="black")+
  scale_size(name = "Tons Consumed",labels = sizeBins)
  #scale_color_discrete(name = "AQI")

finalMap
  
  #geom_point(data=mon2Subset$meta, # Airnow and Airsis monitors
  #           aes(x=longitude, y=latitude),
  #           size=4, color="blue")+ # to adjust colors by AQI...
  #geom_text(data=mon2Subset_dailyMax$meta, #call out monitor names (to replace with max value during period)
  #          aes(x=longitude, y=latitude, label=paste("  Max:",mon2Subset_dailyMaxMax)),
  #          hjust=0,color="blue")+
  #geom_text(data=janice_SMA, # call out ignition times on Rx burns
  #          aes(x=Longitude, y=Latitude, label=paste("   Ignition:",Ignition.time)),
  #          hjust=0,color="orange")
  #  geom_text(aes(x=longitude,y=latitude,data=airsis_monitorsSubset$meta),label = paste("value goes here: ",max(4)), angle = 45, hjust = 0, color = "orange")+

# suppressWarnings(expr)

}















# EXTRAS --------

#coordBounds <- initialMap$data

# Use the following to bypass the subsetting, in case this step is causing problems...
# bluesky_eventsSubset <- bluesky_events
# janice_SMASubset <- janice_SMA
# airnow_monitorsSubset <- airnow_monitors
# airsis_monitorsSubset <- airsis_monitors

# # subset the airnow and airsis ws_monitor objects by time (unlear if this step is needed)
# airnow_monitorsSubset <- monitor_subset(airnow_monitors,tlim=tlim)
# airsis_monitorsSubset <- monitor_subset(airsis_monitors,tlim=tlim)

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

# # Create combined ws_monitor object of airnow and airsis monitors for plotting (to develop further later)
# mon2 <- monitor_combine(list(airsis_monitors,airnow_monitors))
# mon2Subset <- monitor_subset(mon2,tlim=tlim)

# SUBSET DATA BY MAP SCALE -------------

# for each of the subset lines below, may want to add some logic to warn or handle data a certain way
# if no data is found within the given bounds...

# ------

#monSubset_maxDailyMax <- max(monSubset_dailyMax$data[,2])

#mon2Subset_dailyMax <- monitor_dailyStatistic(mon2Subset, max)
#maxDailiyMax <- max(dailyMax$data[,2])
#mon2Subset_hourlyMax <- apply(mon2Subset$data,2,max,na.rm=TRUE)


# THOUGHTS ----------

# Should we center on a specific fire, rather than on a specific monitor?
# Let's include monitor IDs