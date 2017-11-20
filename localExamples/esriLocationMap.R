# ESRI

# OLD:  http://help.arcgis.com/en/arcgisserver/10.0/apis/rest/export.html
# NEW:  http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Export_Map/02r3000000v7000000/

library(dplyr)
library(stringr)

# How to decide which lat/lon to use for bbox? 
# If we want scales to correspond to googleMaps zoom level, use this info:
# * google maps tiles are 256 pixels wide
# * zoom level 0 includes all 360 degrees
# * every time zoom level increases, scale halves. Thus, for degrees EW:
# pixels/degree = 256pixels*2^zoomLevel/360degrees 
# degrees/pixel = 360degrees/(256pixels*2^zoomLevel)
# Source: https://gis.stackexchange.com/questions/7430/what-ratio-scales-do-google-maps-zoom-levels-correspond-to
# Not sure how this corresponds to latitude values, and if it is correct at all latitudes (might depend on projection?)

# target lat/lon: 47.427498, -120.315141

# maptype options: http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Basemaps/02r3000001mt000000/
# NatGeo_World_Map
# World_Street_Map
# World_Topo_Map
# Specialty/DeLorme_World_Base_Map

downloadEsriLocationMap <- function(latitude, longitude, maptype = "World_Street_Map", zoom = 9, size = 640, destination = "~/Desktop/esrimaps/map.png"){
  
  degreesPerPixel <- 360/(256*2^zoom) #degrees EW
  targetLat <- latitude
  targetLon <- longitude
  lonlo <- targetLon - degreesPerPixel*size/2
  lonhi <- targetLon + degreesPerPixel*size/2 
  
  # Can I just set a very small range for latitude, and let esri choose the range based on the size? 
  latlo <- targetLat 
  lathi <- targetLat
  bbox <- paste(lonlo, latlo, lonhi, lathi, sep = ",")
  baseurl <- paste0("http://server.arcgisonline.com/arcgis/rest/services/", maptype, "/MapServer/export")
  url <- paste0(baseurl, "?bbox=", bbox, "&bboxSR=4326&size=",as.character(size),",",as.character(size), "&f=image")
  
  download.file(url, destination)
  
}


# Map for Wenatchee
airnow <- airnow_loadLatest()
wenatchee <- monitor_subset(airnow, monitorIDs = "530070011")

downloadEsriLocationMap(wenatchee$meta$latitude, wenatchee$meta$longitude, zoom = 9, size = 250, 
                        maptype = "World_Street_Map", destination = "~/Desktop/esrimaps/worldstreet.png")

downloadEsriLocationMap(wenatchee$meta$latitude, wenatchee$meta$longitude, zoom = 9, size = 250, 
                        maptype = "NatGeo_World_Map", destination = "~/Desktop/esrimaps/natgeo.png")

downloadEsriLocationMap(wenatchee$meta$latitude, wenatchee$meta$longitude, zoom = 9, size = 250, 
                        maptype = "World_Topo_Map", destination = "~/Desktop/esrimaps/worldtopo.png")

downloadEsriLocationMap(wenatchee$meta$latitude, wenatchee$meta$longitude, zoom = 9, size = 250, 
                        maptype = "Specialty/DeLorme_World_Base_Map", destination = "~/Desktop/esrimaps/delorme.png")


# it seems that mapScale parameter should allow me to specify the scale of the map, no matter the bbox (so maybe could
# set bbox = lon,lat,lon,lat ) BUT whenever mapScale is specified, the map is centered around (0,0)


#https://groups.google.com/forum/#!topic/Google-Maps-API/IF0nqO_6yVg
#
#zoom = 11; 
#mapSize = 640; 
#
#power = 2^zoom = 2^11 = 2048; 
#realWidth = 256 * power = 256 * 2048 = 524288; 
#oneDegree = realWidth / 360 = 524288 / 360 = 1456.355556; 
#
#=> onePixel = mapSize / oneDegree = 640 / 1456.355556 = 0.439453125° 

#Pixels per degree-EW = cos(lat) * pixels per degree-NS ?? 

