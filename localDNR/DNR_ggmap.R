# This function should use the ggmap package to create a map with the following features:
#   arguments = (monitorName='Plain', tlim=c(20160916,20160920))
#   terrain map centered on a monitor
#   display monitor as a filled circle with a color indicating highest daily average during tlim
#   display monitor highest hourly value during tlim as text to the right of monitor circle
#   display prescribed burn locations from janice_SMA during tlim as open red triangles sized by Tons Consumed (FS)
#   display bluesky_events hotspots during tlim as small open orange(?) triangles

# THOUGHTS ----------

# Should we center on a specific fire, rather than on a specific monitor?
# Let's include monitor IDs

library(PWFSLSmoke)
library(ggmap)

# LOAD DATA ------------
# Need to manually load data for now, per the following:

# setwd("~/Projects/PWFSLSmoke")
# source("/localDNR/DNR_ingestData.R") #DOESN'T WORK...?!?!?! Do this manually for now...

# SETTINGS -------------

size <- 5
zoom <- 9

# Temporary variables for testing. Eventually will call in variables using function.
tlim<-c(20160916,20160920)

# Assign lat/lon for map center, based on monitor name passed in
monitorName<-'Plain'
monLat <- airsis_monitors$meta$latitude[which(monitorDict[[monitorName]]==airsis_monitors$meta$monitorID)]
monLon <- airsis_monitors$meta$longitude[which(monitorDict[[monitorName]]==airsis_monitors$meta$monitorID)]

# monitorID <- monitorDict[[monitorName]] # in case this is needed later...

monitorLocation <- c(-120.66450,47.76862) # Lon/Lat (rather than Lat/Lon)
#monLat <- monitorLocation[2]
#monLon <- monitorLocation[1]






# CREATE RAW MAP RASTER -------------
initialMap <- get_map(location=c(monLon,monLat), maptype="terrain", zoom=zoom)
initialMap <- ggmap(initialMap)

coordBounds <- initialMap$data

# SUBSET DATA BY MAP SCALE -------------

# for each of the subset lines below, may want to add some logic to warn or handle data a certain way
# if no data is found within the given bounds...

subset_bluesky_events <- subset(bluesky_events,
                                bluesky_events$latitude<max(coordBounds$lat)&
                                bluesky_events$latitude>min(coordBounds$lat)&
                                bluesky_events$longitude<max(coordBounds$lon)&
                                bluesky_events$longitude>min(coordBounds$lon))

subset_janice_SMA <- subset(janice_SMA,
                            janice_SMA$Latitude<max(coordBounds$lat)&
                            janice_SMA$Latitude>min(coordBounds$lat)&
                            janice_SMA$Longitude<max(coordBounds$lon)&
                            janice_SMA$Longitude>min(coordBounds$lon))

subset_airnow_monitors <- monitor_subset(airnow_monitors,
                                         xlim=c(min(coordBounds$lon),max(coordBounds$lon)),
                                         ylim=c(min(coordBounds$lat),max(coordBounds$lat)))

subset_airsis_monitors <- monitor_subset(airsis_monitors,
                                         xlim=c(min(coordBounds$lon),max(coordBounds$lon)),
                                         ylim=c(min(coordBounds$lat),max(coordBounds$lat)))

# CREATE FINAL MAP
finalMap <- initialMap+
  ggtitle("Here's a Title")+
  #geom_point(aes(x=monLon,y=monLat,color="red", size=size)) + # plot center (will eventuall be monitor)
  
  geom_point(aes(x=longitude, y=latitude),data=subset(subset_bluesky_events,subset_bluesky_events$type=="RX"),size=2,shape=2,color="red")+
  geom_point(aes(x=longitude, y=latitude),data=subset(subset_bluesky_events,subset_bluesky_events$type!="RX"),size=2,shape=2,color="brown")+
  geom_point(aes(x=Longitude, y=Latitude),data=subset_janice_SMA,size=2,color="yellow")+
  geom_point(aes(x=longitude, y=latitude),data=subset_airsis_monitors$meta,size=4,color="blue")+
  geom_point(aes(x=longitude, y=latitude),data=subset_airnow_monitors$meta,size=4,color="green")
  #geom_text(aes(x=monLon,y=monLat,label = "  words"), angle = 45, hjust = 0, color = "orange")+
  #scale_size_area(breaks = 2, labels = "Here's a label", name = "Legend Title...") #legend

finalMap
