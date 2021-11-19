#' @keywords plotting
#' @export
#' @import graphics
#' @title Add Icons to a Map or RgoogleMap Plot
#' @param color marker color: 'red', 'green', 'yellow', 'orange', or 'blue'. Also includes AQI
#' category colors, specified 'AQI[number]' eg. 'AQI1'
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param map optional RgoogleMaps map object or Raster* object
#' @param expansion icon expansion factor. Ignored if width and height are specified.
#' @param ... arguments passed on to \code{rasterImage}
#' @description Adds a marker to a plot or \code{map} -- an RgoogleMaps map object or Raster* object.
#'
#' @examples
#' \dontrun{
#' library(PWFSLSmoke)
#'
#' monitor_map(Camp_Fire)
#'
#' addMarker(
#'   longitude = -121.437222,
#'   latitude = 39.810278,
#'   color = "red",
#'   expansion = 1
#' )
#' }

addMarker <- function(
  longitude,
  latitude,
  color = "red",
  map = NULL,
  expansion = 1,
  ...
) {


  markerPath <- paste0("marker_",color,".png")

  pngFile <- base::system.file("icons", markerPath, package="PWFSLSmoke")

  if ( pngFile == "" ) {
    stop("Cannot find package file 'inst/icons/",markerPath,"'")
  }

  # Read in the png file
  marker <- png::readPNG(pngFile)

  if ( !is.null(map) ) {

    if ( !any( c("staticMap", "RasterBrick", "RasterLayer", "RasterStack") %in% class(map) )  ) {
      stop("'map' argument is not of class 'staticMap' or 'Raster*'")
    }
  }

  if ( "staticMap" %in% class(map) ) {
    #RgoogleMap

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

    # Get lat/lon coordinates for marker dimensions
    zoom <- map$zoom

    degreesPerPixelEW <- 360/(256*2^zoom)
    degreesPerPixelNS <- degreesPerPixelEW*cos(pi/180*latitude) # R does trigonometry in radians


    width <- degreesPerPixelEW*dim(marker)[2]*expansion
    height <- degreesPerPixelNS*dim(marker)[1]*expansion

    left <- longitude - width/2
    right <- longitude + width/2
    bottom <- latitude
    top <- latitude+height

    # Get plot coordinates

    newXY <- RgoogleMaps::LatLon2XY.centered(map, c(bottom, top), c(left, right))
    bottom <- newXY$newY[seq_along(latitude)]
    top <- newXY$newY[-(seq_along(latitude))]
    left <- newXY$newX[seq_along(longitude)]
    right <- newXY$newX[-(seq_along(longitude))]


  } else if ( any( c("staticMap", "RasterBrick", "RasterLayer", "RasterStack") %in% class(map) ) ) {
    # Raster*

    # limit longitude, latitude to those within bounding box
    lon_lo <- map@extent@xmin
    lon_hi <- map@extent@xmax
    lat_lo <- map@extent@ymin
    lat_hi <- map@extent@ymax

    lonMask <- longitude >= lon_lo & longitude <= lon_hi
    latMask <- latitude >= lat_lo & latitude <= lat_hi
    goodMask <- lonMask & latMask

    longitude <- longitude[goodMask]
    latitude <- latitude[goodMask]


    # Get lat/lon coordinates for marker dimensions
    degreesPerPixelEW <- raster::res(map)[1]
    degreesPerPixelNS <- raster::res(map)[2]


    width <- degreesPerPixelEW*dim(marker)[2]*expansion
    height <- degreesPerPixelNS*dim(marker)[1]*expansion

    left <- longitude - width/2
    right <- longitude + width/2
    bottom <- latitude
    top <- latitude+height

  } else {
    # basic plot from maps::map()

    # limit longitude, latitude to those within bounding box
    usr <- par("usr")
    lon_lo <- usr[1]
    lon_hi <- usr[2]
    lat_lo <- usr[3]
    lat_hi <- usr[4]

    lonMask <- longitude >= lon_lo & longitude <= lon_hi
    latMask <- latitude >= lat_lo & latitude <= lat_hi
    goodMask <- lonMask & latMask

    longitude <- longitude[goodMask]
    latitude <- latitude[goodMask]

    # Get width and height

    degreesPerInchEW <- (usr[2]-usr[1]) %% 360 / par("pin")[1]
    degreesPerInchNS <- (usr[4]-usr[3]) %% 360 / par("pin")[2]

    # estimate conversion from inch to pixel (assuming dpi ~ 96)
    degreesPerPixelEW <- degreesPerInchEW/96
    degreesPerPixelNS <- degreesPerInchNS/96

    width <- degreesPerPixelEW*dim(marker)[2]*expansion
    height <- degreesPerPixelNS*dim(marker)[1]*expansion

    left <- longitude - width/2
    right <- longitude + width/2
    bottom <- latitude
    top <- latitude+height

  }


  argsList <- list(...)

  argsList$image <- marker
  argsList$xleft <- left
  argsList$ybottom <- bottom
  argsList$xright <- right
  argsList$ytop <- top

  do.call(graphics::rasterImage, argsList)

}
