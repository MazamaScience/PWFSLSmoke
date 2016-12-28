#' @keywords ws_monitor
#' @export
#' @import graphics
#' @title Create Map of ws_monitor Object and Surrounding Events
#' @param ws_monitor ws_monitor object
#' @param monitorIcon optional icon to use in lieu of default blue paddle
#' @param eventLocations optional dataframe containing longitude and latitude data for events (e.g. fires)
#' @param eventIcon icon for optional events
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param zoom map zoom level
#' @param maptype map type
#' @description Creates a map of a ws_monitor object with icons indicating monitor(s) and surrounding events, if 
#' specified.
#' @examples
#' \dontrun{
#' ws_monitor <- airsis_load(20150901, 20150930)
#' monitor <- ws_monitor$meta$monitorID[3]
#' monitorPlot_timeOfDaySpaghetti(ws_monitor, monitor,tlim=c(20150914,20150930))
#' }

# ===== PRELIM STUFF ==========================

# NOTE: Comment the following out when pushed to package, but leave here for quick reference
# if ( FALSE ) {
# 
#   library(PWFSLSmoke)
#   library(RgoogleMaps)
#   library(png)
# 
#   #Set WD to load sample data
#   setwd("~/Projects/PWFSLSmoke/localData") #work
#   setwd("~/Projects/Mazama/PWFSLSmoke/localData") #home
# 
#   load("airnow_monitors.Rdata")
#   load("airsis_monitors.Rdata")
#   monitorList <- PWFSLSmoke::monitor_combine(list(airnow_monitors,airsis_monitors))
# 
#   rm(airnow_monitors)
#   rm(airsis_monitors)
# 
#   width <- 250
#   height <- 250
#   zoom <- 9
#   maptype <- 'terain'
# 
#   # icons (defaults - use readPNG() to define alternates if wish to use icons other than the defaults)
#   icon_paddle <- png::readPNG('localData/pin1.png') # from http://www.clker.com/clipart-map-pin-1.html
#   icon_fires <- png::readPNG('localData/flame2.png') # from DNR project folder
# 
# }

# ===== ACTUAL STUFF ==========================

###############################################################################
#
# This function creates a map of a monitor and surrounding events (e.g. fires).
# Users can specify the icon(s) they want to use for the monitor and/or events.
#
###############################################################################

monitorPlot_locationMap <- function(ws_monitor,
                                    monitorIcon=NULL,
                                    eventLocations=NULL,
                                    eventIcon=NULL,
                                    width=250,
                                    height=250,
                                    zoom=9,
                                    maptype='terain') {
  
  # ----- Style ---------------------------------------------------------------
  
  # Events as red-outlined, gray triangles (unless otherwise overwritten by icon)
  cex_events <- 1.2
  pch_events <- 17
  col_events <- 'gray60'
  pch_eventsBorder <- 2
  col_eventsBorder <- 'red'
  lwd_eventsBorder <- 2
  
  # icons (defaults - use readPNG() to define alternates if wish to use something other than default)
  icon_paddle <- png::readPNG('localData/pin1.png') # from http://www.clker.com/clipart-map-pin-1.html
  
  # icon expansion
  iconExpansion_monitor <- .1
  iconExpansion_events <- .1
  
  # ----- Data Preparation ----------------------------------------------------
  
  lat <- base::mean(ws_monitor$meta$latitude)
  lon <- base::mean(ws_monitor$meta$longitude)
  
  # ----- Generate map --------------------------------------------------------
  
  myMap <- RgoogleMaps::GetMap(center=c(lat,lon), size=c(height,width), zoom=zoom, maptype=maptype);
  
  RgoogleMaps::PlotOnStaticMap(myMap)
  
  # Plot events first
  if ( !is.null(eventLocations) ) {
    if ( is.null(eventIcon) ) {
      RgoogleMaps::PlotOnStaticMap(myMap,eventLocations$latitude,eventLocations$longitude,
                                   add=TRUE,
                                   cex=cex_events, pch=pch_events, col=col_events)
      RgoogleMaps::PlotOnStaticMap(myMap,eventLocations$latitude,eventLocations$longitude,
                                   add=TRUE,
                                   cex=cex_events, pch=pch_eventsBorder, col=col_eventsBorder, lwd=lwd_eventsBorder)
    } else { # TODO: add logic to determine WHICH icon to plot, if more than one can be specified
      addIcon(myMap,eventIcon,eventLocations$longitude,eventLocations$latitude)
    }
  }
  
  # Plot monitor(s) on top
  if ( is.null(monitorIcon) ) {
    addIcon(myMap, icon_paddle, ws_monitor$meta$longitude, ws_monitor$meta$latitude, expansion=iconExpansion_monitor, pos=3) #could build in icon expansion, or logic re: pos depending on icon type
  } else {
    addIcon(myMap, monitorIcon, ws_monitor$meta$longitude, ws_monitor$meta$latitude, expansion=iconExpansion_monitor, pos=0) #could build in icon expansion, or logic re: pos depending on icon type
  }
  
}

# ----- Helper Functions ------------------------------------------------------

addIcon <- function(map, icon, lon, lat, expansion=0.1, pos=0) {
  
  # limit lon, lat to those within bounding box
  lon_lo <- map$BBOX$ll[,'lon']
  lon_hi <- map$BBOX$ur[,'lon']
  lat_lo <- map$BBOX$ll[,'lat']
  lat_hi <- map$BBOX$ur[,'lat']
  
  lonMask <- lon >= lon_lo & lon <= lon_hi
  latMask <- lat >= lat_lo & lat <= lat_hi
  goodMask <- lonMask & latMask
  
  lon <- lon[goodMask]
  lat <- lat[goodMask]
  
  # Calculate final icon size
  icon_height <- dim(icon)[1] * expansion
  icon_width <- dim(icon)[2] * expansion
  
  # Calcualte "nudge" based on pos
  nudge_x <- 0
  nudge_y <- 0
  if ( pos == 1 ) { # below
    nudge_x <- 0
    nudge_y <- -icon_height/2
  } else if ( pos == 2 ) { # left of
    nudge_x <- -icon_width/2
    nudge_y <- 0
  } else if ( pos == 3 ) { # above
    nudge_x <- 0
    nudge_y <- icon_height/2
  } else if ( pos == 4 ) { # right of
    nudge_x <- icon_width/2
    nudge_y <- 0
  }
  
  # Get plot coordinates
  newXY <- RgoogleMaps::LatLon2XY.centered(map, lat, lon)
  x <- newXY$newX
  y <- newXY$newY
  
  graphics::rasterImage(icon,
                        x - icon_width/2 + nudge_x,
                        y - icon_height/2 + nudge_y,
                        x + icon_width/2 + nudge_x,
                        y + icon_height/2 + nudge_y)
  
}
