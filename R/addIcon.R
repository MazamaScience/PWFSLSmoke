#' @export
#' @import graphics
#' @title Add Icons to a Map or RgoogleMap Plot
#' @param icon object to be plotted
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param map optional RgoogleMaps map object
#' @param expansion icon expansion factor
#' @param pos position of icon relative to location (0=center, 1=bottom, 2=left, 3=top,4=right)
#' @description Adds an icon to \code{map} -- an RgoogleMaps map object.
#' The following icons are available:
#'
#' \itemize{
#' \item{\code{orangeFlame}}{ -- yellow-orange flame}
#' \item{\code{redFlame}}{ -- orange-red flame}
#' }
#'
#' You can use other .png files as icons by passing an absolute path as the \code{icon} argument.
#'
#' @note For RgoogleMaps, the \code{expansion} will be ~ 0.1 while for basic plots it may need
#' to be much smaller, perhaps ~ 0.001.
#' @examples
#' \dontrun{
#' library(PWFSLSmoke)
#'
#' monitor_map(Camp_Fire)
#'
#' addIcon(
#'   "orangeFlame",
#'   longitude = -121.437222,
#'   latitude = 39.810278,
#'   expansion = 0.003
#' )
#' }


addIcon <- function(
  icon,
  longitude,
  latitude,
  map = NULL,
  expansion = 0.1,
  pos = 0
) {

  # Test for absolute path
  if ( dirname(icon) == "." ) {
    # package icon
    # let users specify either "orangeFlame" or "orangeFlame.png"
    icon <- stringr::str_replace(icon,".png","")
    icon <- paste0(icon,".png")
    pngFile <- base::system.file("icons", icon, package="PWFSLSmoke")
  } else {
    # non-package icon, must be valid absolute path
    pngFile <- icon
  }

  if ( pngFile == "" ) {
    stop("Cannot find package file 'inst/icons/",icon,"'")
  }

  # Read in the png file
  icon <- png::readPNG(pngFile)

  if ( !is.null(map) ) {
    # RgoogleMap

    if ( !"staticMap" %in% class(map) ) {
      stop("'map' argument is not of class 'staticMap'")
    }

    # limit longitude, latitude to those within bounding box
    lon_lo <- map$BBOX$ll[,'lon']
    lon_hi <- map$BBOX$ur[,'lon']
    lat_lo <- map$BBOX$ll[,'lat']
    lat_hi <- map$BBOX$ur[,'lat']

    lonMask <- longitude >= lon_lo & longitude <= lon_hi
    latMask <- latitude >= lat_lo & latitude <= lat_hi
    goodMask <- lonMask & latMask

    longitude <- longitude[goodMask]
    latitude <- latitude[goodMask]

    # Get plot coordinates
    newXY <- RgoogleMaps::LatLon2XY.centered(map, latitude, longitude)
    x <- newXY$newX
    y <- newXY$newY

  } else {
    # basic plot from maps::map()
    x <- longitude
    y <- latitude

  }

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

  graphics::rasterImage(icon,
                        x - icon_width/2 + nudge_x,
                        y - icon_height/2 + nudge_y,
                        x + icon_width/2 + nudge_x,
                        y + icon_height/2 + nudge_y)

}
