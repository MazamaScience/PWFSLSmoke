#' @export
#' @import graphics
#' @title Add an Icon to a RgoogleMap Plot
#' map, icon, lon, lat, expansion=0.1, pos=0
#' @param map RgoogleMaps map object
#' @param icon object to be plotted
#' @param lon vector of longitudes
#' @param lat vector of latitudes
#' @param expansion icon expansion factor
#' @param pos position of icon relative to lon/lat (0=center, 1=bottom, 2=left, 3=top,4=right)
#' @description Internal function aclled by monitorGoogleMap~ functions.

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
